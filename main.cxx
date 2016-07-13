/* Copyright © 2016 Taylor C. Richberger <taywee@gmx.com>
 * This code is released under the license described in the LICENSE file
 */

#include <iostream>
#include <iomanip>
#include <set>
#include <list>
#include <vector>
#include <algorithm>
#include <memory>
#include <numeric>
#include <stack>
#include <sstream>
#include <functional>
#include <unordered_map>

#include <args.hxx>
#include <Magick++.h>

#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include "color.hxx"

static Field ImageToField(const std::string filename, size_t codelsize, Color unknown);
static void PrintField(const Field &field, const Map &map);

int main(const int argc, const char **argv)
{
    args::ArgumentParser parser("A LLVM piet compiler, which can compile a Piet program "
            "into LLVM IR, LLVM bitcode, native assembly, native object code, or an "
            "executable, as well as run the optimized JIT compiled Piet program immediately.");
    args::HelpFlag help(parser, "help", "Display this help menu", {'h', "help"});
    args::ValueFlag<size_t> cs(parser, "size", "Set the codel size manually (0 tries to auto-detect, which usually works, but may be imperfect in certain scenarios)", {"cs", "codel-size"}, 0);
    //args::Flag trace(parser, "trace", "Trace program run", {'t', "trace"});
    args::Flag unknownWhite(parser, "unknown-white", "Make unknown colors white", {"unknown-white"});
    args::Flag unknownBlack(parser, "unknown-black", "Make unknown colors black", {"unknown-black"});
    args::ValueFlag<size_t> startstacksize(parser, "size", "Specify the starting stack size", {'s', "stack"}, 1);
    args::ValueFlag<std::string> prompt(parser, "prompt", "Prompt for input operations", {'p', "prompt"}, "? ");
    args::Group required(parser, "", args::Group::Validators::All);
    args::Positional<std::string> input(required, "input", "Input file to use");

    try
    {
        parser.ParseCLI(argc, argv);
        if (args::get(startstacksize) < 1)
        {
            args::get(startstacksize) = 1;
        }
    }
    catch (const args::Help &)
    {
        std::cout << parser;
        return 0;
    }
    catch (const args::ValidationError &e)
    {
        if (!input)
        {
            std::cerr << "input is a required argument" << std::endl;
        }
        std::cerr << parser;
        return 1;
    }
    catch (const args::Error &e)
    {
        std::cerr << e.what() << std::endl;
        std::cerr << parser;
        return 1;
    }

    try {
        const Field field = ImageToField(args::get(input), args::get(cs), (unknownWhite ? Color::White : (unknownBlack ? Color::Black : Color::Unknown)));
        Map map;

        for (size_t y = 0; y < field.size(); ++y)
        {
            const auto &row = field[y];
            for (size_t x = 0; x < row.size(); ++x)
            {
                const auto &codel = row[x];
                if (map.find(Coords{x, y}) == map.end())
                {
                    // Queue-based flood fill algorithm: https://en.wikipedia.org/wiki/Flood_fill
                    std::unordered_set<Coords> processed;
                    for (std::unordered_set<Coords> queue{Coords{x, y}}; !queue.empty();)
                    {
                        const auto it = queue.begin();
                        const auto n = *it;
                        queue.erase(it);
                        const auto &x = std::get<0>(n);
                        const auto &y = std::get<1>(n);

                        const auto &ncodel = field[y][x];
                        // Check this codel color against other existing ones
                        if (ncodel == codel)
                        {
                            processed.emplace(n);

                            // West
                            if (x > 0)
                            {
                                const Coords coord{x - 1, y};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                            // East
                            if (x < (row.size() - 1))
                            {
                                const Coords coord{x + 1, y};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                            // North
                            if (y > 0)
                            {
                                const Coords coord{x, y - 1};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                            // South
                            if (y < (field.size() - 1))
                            {
                                const Coords coord{x, y + 1};
                                if (processed.find(coord) == processed.end() && queue.find(coord) == queue.end())
                                {
                                    queue.emplace(coord);
                                }
                            }
                        }
                    }
                    // The color blocks live entirely in the map
                    auto colorBlock = std::make_shared<ColorBlock>(codel, processed.size(), std::list<Coords>(std::begin(processed), std::end(processed)));

                    for (const auto &coord: processed)
                    {
                        map.insert(std::make_pair(coord, colorBlock));
                    }
                }
            }
        }

        std::unordered_set<std::shared_ptr<ColorBlock>> colorBlocks;
        std::transform(std::begin(map), std::end(map), std::inserter(colorBlocks, std::end(colorBlocks)), [](const MapItem &item) {return item.second;});

        // Call setneighbors
        using namespace std::placeholders;
        for (auto &colorBlock: colorBlocks)
        {
            switch (colorBlock->codel.color)
            {
                case Color::Red:
                case Color::Yellow:
                case Color::Green:
                case Color::Cyan:
                case Color::Blue:
                case Color::Magenta:
                    colorBlock->SetNeighbors(field, map);
                    break;
                default:
                    break;
            }
        }

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder(context);
        llvm::Module module(args::get(input), context);

        llvm::Type *sizet_type;
        switch(sizeof(size_t))
        {
            case 16:
                sizet_type = llvm::Type::getInt128Ty(context);
                break;

            case 8:
                sizet_type = llvm::Type::getInt64Ty(context);
                break;
                
            case 4:
                sizet_type = llvm::Type::getInt32Ty(context);
                break;

            case 2:
                sizet_type = llvm::Type::getInt16Ty(context);
                break;

            case 1:
                sizet_type = llvm::Type::getInt8Ty(context);
                break;

            default:
                throw std::runtime_error("Just what architecture are you running, buddy?");
                break;
        }

        //printf
        llvm::FunctionType *printftype = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {llvm::Type::getInt8PtrTy(context)}, true);
        llvm::Function *printf = llvm::Function::Create(printftype, llvm::Function::ExternalLinkage, "printf", &module);

        //scanf
        llvm::FunctionType *scanftype = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {llvm::Type::getInt8PtrTy(context)}, true);
        llvm::Function *scanf = llvm::Function::Create(scanftype, llvm::Function::ExternalLinkage, "printf", &module);

        //getchar
        llvm::FunctionType *getchartype = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);
        llvm::Function *getchar = llvm::Function::Create(getchartype, llvm::Function::ExternalLinkage, "getchar", &module);

        //putchar
        llvm::FunctionType *putchartype = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {llvm::Type::getInt32Ty(context)}, false);
        llvm::Function *putchar = llvm::Function::Create(putchartype, llvm::Function::ExternalLinkage, "putchar", &module);

        //realloc
        llvm::FunctionType *realloctype = llvm::FunctionType::get(llvm::Type::getInt64PtrTy(context), {llvm::Type::getInt64PtrTy(context), sizet_type}, false);
        llvm::Function *realloc = llvm::Function::Create(realloctype, llvm::Function::ExternalLinkage, "realloc", &module);

        //popstack
        // value, stack, pointer to current stack size
        llvm::FunctionType *popstacktype = llvm::FunctionType::get(llvm::Type::getInt64Ty(context), {llvm::Type::getInt64PtrTy(context), llvm::Type::getInt64PtrTy(context)}, false);
        llvm::Function *popstack = llvm::Function::Create(popstacktype, llvm::Function::InternalLinkage, "popstack", &module);
        {
            llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "", popstack);
            builder.SetInsertPoint(bb);
            std::vector<llvm::Value*> args;
            for (auto &arg: popstack->args())
            {
                args.push_back(&arg);
            }
            auto stack = args[0];
            auto stacksize = args[1];

            auto empty = llvm::BasicBlock::Create(context, "empty", popstack);
            auto nempty = llvm::BasicBlock::Create(context, "nempty", popstack);

            auto cond = builder.CreateICmpEQ(builder.getInt64(0), builder.CreateLoad(stacksize));
            builder.CreateCondBr(cond, empty, nempty);

            {
                builder.SetInsertPoint(empty);
                builder.CreateRet(builder.getInt64(0));
            }
            {
                builder.SetInsertPoint(nempty);
                builder.CreateStore(builder.CreateSub(builder.CreateLoad(stacksize), builder.getInt64(1)), stacksize);
                builder.CreateRet(builder.CreateLoad(builder.CreateGEP(stack, builder.CreateLoad(stacksize))));
            }
        }

        ////pushstack
        //// value, stack, pointer to current stack size, pointer to stack reserved size
        llvm::FunctionType *pushstacktype = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {llvm::Type::getInt64Ty(context), llvm::PointerType::get(llvm::Type::getInt64PtrTy(context), 0), llvm::Type::getInt64PtrTy(context), llvm::Type::getInt64PtrTy(context)}, false);
        llvm::Function *pushstack = llvm::Function::Create(pushstacktype, llvm::Function::InternalLinkage, "pushstack", &module);
        {
            llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "", pushstack);
            builder.SetInsertPoint(bb);
            std::vector<llvm::Value*> args;
            for (auto &arg: pushstack->args())
            {
                args.push_back(&arg);
            }
            auto value = args[0];
            value->setName("value");
            auto stack = args[1];
            stack->setName("stack");
            auto stacksize = args[2];
            stacksize->setName("stacksize");
            auto reservedsize = args[3];
            reservedsize->setName("reservedsize");

            auto full = llvm::BasicBlock::Create(context, "full", pushstack);
            auto push = llvm::BasicBlock::Create(context, "push", pushstack);

            // The stack is full
            auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), builder.CreateLoad(reservedsize));
            builder.CreateCondBr(cond, full, push);

            {
                builder.SetInsertPoint(full);
                builder.CreateStore(builder.CreateMul(builder.CreateLoad(reservedsize), builder.getInt64(2)), reservedsize);
                llvm::Value *realcall = builder.CreateCall(realloc, {builder.CreateLoad(stack), builder.CreateMul(builder.CreateLoad(reservedsize), builder.getInt64(sizeof(size_t)))});
                builder.CreateStore(realcall, stack, false);
                builder.CreateBr(push);
            }
            {
                builder.SetInsertPoint(push);
                builder.CreateStore(value, builder.CreateGEP(builder.CreateLoad(stack), builder.CreateLoad(stacksize)));
                builder.CreateStore(builder.CreateAdd(builder.CreateLoad(stacksize), builder.getInt64(1)), stacksize);
                builder.CreateRetVoid();
            }
        }

        llvm::FunctionType *mainType = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);
        llvm::Constant* mainC = module.getOrInsertFunction("main", mainType);
        llvm::Function* mainFunc = llvm::dyn_cast<llvm::Function>(mainC);

        llvm::BasicBlock *bb = llvm::BasicBlock::Create(context, "mainblock", mainFunc);
        builder.SetInsertPoint(bb);

        llvm::AllocaInst *dcalloc = builder.CreateAlloca(llvm::Type::getInt8Ty(context), 0, "dc");
        auto i1 = llvm::Type::getInt1Ty(context);
        llvm::AllocaInst *ccalloc = builder.CreateAlloca(i1, 0, "cc");
        llvm::AllocaInst *stackalloc = builder.CreateAlloca(llvm::Type::getInt64PtrTy(context), 0, "stack");
        llvm::AllocaInst *stacksize = builder.CreateAlloca(sizet_type, 0, "stacksize");
        llvm::AllocaInst *stackreserved = builder.CreateAlloca(sizet_type, 0, "stackreserved");
        llvm::Value *realcall = builder.CreateCall(realloc, {llvm::ConstantPointerNull::get(llvm::Type::getInt64PtrTy(context)), llvm::ConstantInt::get(sizet_type, args::get(startstacksize) * sizeof(size_t))});
        builder.CreateStore(realcall, stackalloc, false);
        builder.CreateStore(llvm::ConstantInt::get(sizet_type, 0), stacksize, false);
        builder.CreateStore(llvm::ConstantInt::get(sizet_type, args::get(startstacksize)), stackreserved, false);
        llvm::Value *printdigit = builder.CreateGlobalStringPtr("stack size: %llu, reserved: %llu\n");
        llvm::Value *printresult = builder.CreateGlobalStringPtr("stack size: %llu, reserved: %llu, retvalue: %llu\n");
        auto blank = llvm::BasicBlock::Create(context, "blank", mainFunc);
        llvm::ReturnInst::Create(context, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), blank);




        CC startcc = CC::Left;
        DC startdc = DC::East;
        std::shared_ptr<ColorBlock> startblock = map.find(Coords{0, 0})->second;
        bool exit = false;

        switch (startblock->codel.color)
        {
            case Color::White:
                std::tie(startblock, startdc, startcc, exit) = TraceWhite(Coords{0, 0}, field, map, startdc, startcc);
                break;
            case Color::Black:
                exit = true;
                break;
            default:
                break;
        }
        if (!exit)
        {
            // first set dc and cc
            builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(startdc)), dcalloc, false);
            builder.CreateStore(llvm::ConstantInt::get(i1, static_cast<uint8_t>(startcc)), ccalloc, false);

            std::map<std::shared_ptr<ColorBlock>, llvm::BasicBlock*> blocks;
            for (auto &colorBlock: map)
            {
                auto block = colorBlock.second;
                switch (block->codel.color)
                {
                    case Color::Red:
                    case Color::Yellow:
                    case Color::Green:
                    case Color::Cyan:
                    case Color::Blue:
                    case Color::Magenta:
                        {
                            if (blocks.find(block) == std::end(blocks))
                            {
                                std::ostringstream name;
                                name << "colorblock_" << block->codel << "_" << block->exits[static_cast<size_t>(DC::North)][static_cast<size_t>(CC::Left)];
                                blocks.emplace(block, llvm::BasicBlock::Create(context, name.str(), mainFunc));
                            }
                        }
                        break;
                    default:
                        break;
                }
            }
            // Make main jump to the correct block
            builder.CreateBr(blocks.find(startblock)->second);
            for (auto &colorBlock: blocks)
            {
                auto block = colorBlock.first;
                auto bb = colorBlock.second;
                builder.SetInsertPoint(bb);
                /*if (trace)
                {
                    std::cout << "##########" << std::endl;
                    std::cout << "Codel   : " << block->codel << std::endl;
                    std::cout << "Exits   : " << std::endl;
                    for (auto dc: std::list<DC>{DC::North, DC::East, DC::South, DC::West})
                    {
                        for (auto cc: std::list<CC>{CC::Left, CC::Right})
                        {
                            if (!block->neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)].block.expired())
                            {
                                std::cout << "    " << dc << cc << "  : " << block->exits[static_cast<size_t>(dc)][static_cast<size_t>(cc)] << std::endl;
                            }
                        }
                    }
                    std::cout << "Stack   : ";
                    std::copy(std::begin(stack), std::end(stack), std::ostream_iterator<long long int>(std::cout, " "));
                    std::cout << std::endl;
                }*/
                // If this block has no neighbors, kill it.
                if (std::all_of(std::begin(block->neighbors), std::end(block->neighbors),
                            [](const std::array<ColorBlock::Neighbor, 2> &arr)
                            {
                            return std::all_of(std::begin(arr), std::end(arr),
                                    [](const ColorBlock::Neighbor &n)
                                    {
                                    return n.block.expired();
                                    });
                            }))
                {
                    builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0));
                    continue;
                }
                auto getNeighbor = [](std::array<std::array<ColorBlock::Neighbor, 2>, 4> &neighbors, DC dc, CC cc) -> ColorBlock::Neighbor&
                {
                    bool toggleCC = true;
                    auto &neighbor = neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)];
                    for (neighbor = neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)];
                            neighbor.block.expired();
                            neighbor = neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)])
                    {
                        if (toggleCC)
                            cc = Toggle(cc);
                        else
                            dc = Toggle(dc);
                        toggleCC = !toggleCC;
                    }
                    return neighbor;
                };

                std::map<ColorBlock::Neighbor *, std::list<std::tuple<DC, CC>>> neighbors;
                std::map<std::tuple<DC, CC>, ColorBlock::Neighbor *> rneighbors;
                for (auto dc: std::list<DC>{DC::North, DC::East, DC::South, DC::West})
                {
                    for (auto cc: std::list<CC>{CC::Left, CC::Right})
                    {
                        auto &neighbor = getNeighbor(block->neighbors, dc, cc);
                        neighbors[&neighbor].emplace_back(dc, cc);
                        rneighbors.emplace(std::tuple<DC, CC>{dc, cc}, &neighbor);
                    }
                }
                std::map<ColorBlock::Neighbor *, llvm::BasicBlock*> neighborbbs;
                for(auto &n: neighbors)
                {
                    auto neighbor = n.first;
                    std::ostringstream name;
                    name << "neighbor_" << neighbor->dc << neighbor->cc;
                    neighborbbs.emplace(neighbor, llvm::BasicBlock::Create(context, name.str(), mainFunc));
                }

                auto left = llvm::BasicBlock::Create(context, "left", mainFunc);
                auto right = llvm::BasicBlock::Create(context, "right", mainFunc);

                // Check if cc is left
                auto cond = builder.CreateICmpEQ(builder.getInt1(static_cast<uint8_t>(CC::Left)), builder.CreateLoad(ccalloc));

                // Link all cc and dc combos to matching neighbors
                builder.CreateCondBr(cond, left, right);
                builder.SetInsertPoint(left);
                auto lsw = builder.CreateSwitch(builder.CreateLoad(dcalloc), blank, 4);
                lsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::North)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::North, CC::Left})->second)->second);
                lsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::East)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::East, CC::Left})->second)->second);
                lsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::South)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::South, CC::Left})->second)->second);
                lsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::West)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::West, CC::Left})->second)->second);
                builder.SetInsertPoint(right);
                auto rsw = builder.CreateSwitch(builder.CreateLoad(dcalloc), blank, 4);
                rsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::North)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::North, CC::Right})->second)->second);
                rsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::East)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::East, CC::Right})->second)->second);
                rsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::South)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::South, CC::Right})->second)->second);
                rsw->addCase(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::West)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::West, CC::Right})->second)->second);
                
                for(auto &n: neighbors)
                {
                    auto &neighbor = *n.first;
                    auto newdc = neighbor.dc;
                    auto newcc = neighbor.cc;
                    auto bb = neighborbbs.find(&neighbor)->second;
                    auto nextbb = blocks.find(neighbor.block.lock())->second;
                    builder.SetInsertPoint(bb);
                    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(newdc)), dcalloc);
                    builder.CreateStore(llvm::ConstantInt::get(i1, static_cast<uint8_t>(newcc)), ccalloc);
                    switch (neighbor.operation)
                    {
                        case OP::PUSH:
                            builder.CreateCall(pushstack, {builder.getInt64(block->size), stackalloc, stacksize, stackreserved});
                            builder.CreateCall(printf, {printresult, builder.CreateLoad(stacksize), builder.CreateLoad(stackreserved), builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize})});
                            break;
                        case OP::POP:
                            builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                            break;
                        /*case OP::ADD:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.back();
                                stack.pop_back();
                                const auto second = stack.back();
                                stack.pop_back();
                                stack.push_back(first + second);
                            }
                            break;
                        case OP::SUBTRACT:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.back();
                                stack.pop_back();
                                const auto second = stack.back();
                                stack.pop_back();
                                stack.push_back(second - first);
                            }
                            break;
                        case OP::MULTIPLY:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.back();
                                stack.pop_back();
                                const auto second = stack.back();
                                stack.pop_back();
                                stack.push_back(first * second);
                            }
                            break;
                        case OP::DIVIDE:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.back();
                                stack.pop_back();
                                const auto second = stack.back();
                                stack.pop_back();
                                stack.push_back(second / first);
                            }
                            break;
                        case OP::MOD:
                            if (stack.size() >= 2)
                            {
                                auto first = stack.back();
                                stack.pop_back();
                                auto second = stack.back();
                                stack.pop_back();
                                if (first == 0)
                                {
                                    stack.push_back(second);
                                    stack.push_back(first);
                                    break;
                                }

                                const bool positive = second >= 0;
                                auto result = second % first;

                                if (result < 0 && positive)
                                {
                                    result *= -1;
                                }
                                stack.push_back(result);
                            }
                            break;
                        case OP::NOT:
                            if (stack.size() >= 1)
                            {
                                const auto first = stack.back();
                                stack.pop_back();
                                stack.push_back(!first);
                            }
                            break;
                        case OP::GREATER:
                            if (stack.size() >= 2)
                            {
                                const auto first = stack.back();
                                stack.pop_back();
                                const auto second = stack.back();
                                stack.pop_back();
                                stack.push_back(second > first);
                            }
                            break;
                        case OP::POINTER:
                            if (stack.size() >= 1)
                            {
                                auto first = stack.back();
                                stack.pop_back();
                                if (first > 0)
                                {
                                    dc = static_cast<DC>((static_cast<size_t>(dc) + first) % 4);
                                } else if (first < 0)
                                {
                                    first *= -1;
                                    dc = static_cast<DC>((static_cast<size_t>(dc) + 4 - (first % 4)) % 4);
                                }
                            }
                            break;
                        case OP::SWITCH:
                            if (stack.size() >= 1)
                            {
                                auto first = stack.back();
                                stack.pop_back();
                                if (first < 0)
                                {
                                    first *= -1;
                                }
                                if (first % 2 == 1)
                                {
                                    cc = Toggle(cc);
                                }
                            }
                            break;
                        case OP::DUPLICATE:
                            if (stack.size() >= 1)
                            {
                                stack.push_back(stack.back());
                            }
                            break;
                        case OP::ROLL:
                            if (stack.size() >= 2)
                            {
                                auto amount = stack.back();
                                stack.pop_back();
                                auto depth = stack.back();
                                stack.pop_back();

                                if (depth < 0 || static_cast<size_t>(depth) > stack.size())
                                {
                                    stack.push_back(depth);
                                    stack.push_back(amount);
                                } else if (depth > 0)
                                {
                                    A roll equal to the depth restores the exact same order
                                    amount %= depth;

                                    auto rollend = stack.rbegin();
                                    std::advance(rollend, depth);

                                    auto newtop = stack.rbegin();
                                    std::advance(newtop, amount);

                                    std::rotate(stack.rbegin(), newtop, rollend);
                                }
                            }
                            break;
                        case OP::INN:
                            {
                                std::cout << args::get(prompt);
                                long long int input;
                                std::cin >> input;
                                stack.push_back(input);
                            }
                            break;
                        case OP::INC:
                            {
                                std::cout << args::get(prompt);
                                stack.push_back(std::cin.get());
                            }
                            break;
                        case OP::OUTN:
                            if (stack.size() >= 1)
                            {
                                std::cout << stack.back() << std::flush;
                                stack.pop_back();
                            }
                            break;
                        case OP::OUTC:
                            if (stack.size() >= 1)
                            {
                                std::cout << static_cast<char>(stack.back()) << std::flush;
                                stack.pop_back();
                            }
                            break;
                        case OP::EXIT:
                            {
                                return 0;
                            }
                            break;
                            */
                        default:
                            break;
                    }
                    builder.CreateBr(nextbb);
                }
            }
        }
        module.dump();
        return 0;
    }
    catch (const Magick::Exception &e)
    {
        std::cerr << "Image file " << args::get(input) << " could not be read" << std::endl;
        std::cerr << parser;
        return 1;
    }
    return 0;
}

template <typename Pix>
bool Equals(const Pix &a, const Pix &b)
{
    return a.red == b.red
        && a.green == b.green
        && a.blue == b.blue;
}


Field ImageToField(const std::string filename, size_t codelsize, Color unknown)
{
    Magick::Image image;

    image.read(filename);

    Magick::Pixels pixels(image);

    const auto size = image.size();
    if (codelsize == 0)
    {
        std::set<size_t> linestops{0, size.width(), size.height()};

        // First do each row
        for (size_t y = 0; y < size.height(); ++y)
        {
            auto prevColor = pixels.get(0, y, 1, 1)[0];
            auto row = pixels.get(0, y, size.width(), 1);
            for (size_t x = 0; x < size.width(); ++x)
            {
                auto color = row[x];
                if (!Equals(prevColor, color))
                {
                    prevColor = color;
                    linestops.insert(x);
                }
            }
        }

        // Then each column in the same way
        for (size_t x = 0; x < size.width(); ++x)
        {
            auto prevColor = pixels.get(x, 0, 1, 1)[0];
            auto col = pixels.get(x, 0, 1, size.height());
            for (size_t y = 0; y < size.height(); ++y)
            {
                auto color = col[y];
                if (!Equals(prevColor, color))
                {
                    prevColor = color;
                    linestops.insert(y);
                }
            }
        }

        std::list<size_t> diffs(linestops.size());
        std::adjacent_difference(std::begin(linestops), std::end(linestops), std::begin(diffs));
        diffs.pop_front();
        codelsize = *std::min_element(std::begin(diffs), std::end(diffs));
        std::cout << "autodetected codel size: " << codelsize << std::endl;
    }

    // Each vector member is a row, indexed top down
    Field field = std::vector<std::vector<Codel>>(
            size.height() / codelsize,
            std::vector<Codel>(size.width() / codelsize));

    const std::unordered_map<PixelColor, Codel> colormap{
        {{255, 192, 192}, {Shade::Light, Color::Red}},
            {{255, 0, 0}, {Shade::Normal, Color::Red}},
            {{192, 0, 0}, {Shade::Dark, Color::Red}},
            {{255, 255, 192}, {Shade::Light, Color::Yellow}},
            {{255, 255, 0}, {Shade::Normal, Color::Yellow}},
            {{192, 192, 0}, {Shade::Dark, Color::Yellow}},
            {{192, 255, 192}, {Shade::Light, Color::Green}},
            {{0, 255, 0}, {Shade::Normal, Color::Green}},
            {{0, 192, 0}, {Shade::Dark, Color::Green}},
            {{192, 255, 255}, {Shade::Light, Color::Cyan}},
            {{0, 255, 255}, {Shade::Normal, Color::Cyan}},
            {{0, 192, 192}, {Shade::Dark, Color::Cyan}},
            {{192, 192, 255}, {Shade::Light, Color::Blue}},
            {{0, 0, 255}, {Shade::Normal, Color::Blue}},
            {{0, 0, 192}, {Shade::Dark, Color::Blue}},
            {{255, 192, 255}, {Shade::Light, Color::Magenta}},
            {{255, 0, 255}, {Shade::Normal, Color::Magenta}},
            {{192, 0, 192}, {Shade::Dark, Color::Magenta}},
            {{255, 255, 255}, {Shade::None, Color::White}},
            {{0, 0, 0}, {Shade::None, Color::Black}}};

    for (size_t y = 0, realy = 0; y < size.height(); y += codelsize, ++realy)
    {
        auto row = pixels.get(0, y, size.width(), 1);
        for (size_t x = 0, realx = 0; x < size.width(); x += codelsize, ++realx)
        {
            PixelColor pixelColor(row[x]);
            auto colorit = colormap.find(pixelColor);
            if (colorit == colormap.end())
            {
                if (unknown == Color::Unknown)
                {
                    std::ostringstream problem;
                    problem << "Could not parse image due to unknown color " << pixelColor << " at " << x << 'x' << y << ".  Please fix image or set unknown to white or black in the options";
                    throw std::runtime_error(problem.str());
                }
                field[realy][realx] = {Shade::None, unknown};
            } else
            {
                field[realy][realx] = colorit->second;
            }
        }
    }
    return field;
}


void PrintField(const Field &field, const Map &map)
{
    for (size_t y = 0; y < field.size(); ++y)
    {
        const auto &row = field[y];
        for (size_t x = 0; x < row.size(); ++x)
        {
            const auto it = map.find(Coords{x, y});
            if (it == std::end(map))
            {
                std::cout << "   ";
            } else
            {
                std::cout << static_cast<char>(033) << '[';
                const auto &codel = it->second->codel;
                switch (codel.color)
                {
                    case Color::Black:
                        std::cout << "40";
                        break;
                    case Color::Red:
                        std::cout << "41";
                        break;
                    case Color::Green:
                        std::cout << "42";
                        break;
                    case Color::Yellow:
                        std::cout << "43";
                        break;
                    case Color::Blue:
                        std::cout << "44";
                        break;
                    case Color::Magenta:
                        std::cout << "45";
                        break;
                    case Color::Cyan:
                        std::cout << "46";
                        break;
                    case Color::White:
                        std::cout << "47";
                        break;
                    default:
                        std::cout << "47";
                        break;
                }
                std::cout << 'm';
                std::cout << std::setfill('0') << std::setw(3) << it->second->size;
            }
            std::cout << ' ';
            std::cout << static_cast<char>(033) << "[39;49m";
        }
        std::cout << '\n';
    }
}
