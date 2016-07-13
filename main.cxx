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

const static bool CHARISSIGNED = (static_cast<char>(0) - 1 > 0);

static Field ImageToField(const std::string filename, size_t codelsize, Color unknown);
static void PrintField(const Field &field, const Map &map);

int main(const int argc, const char **argv)
{
    args::ArgumentParser parser("A LLVM piet compiler, which can compile a Piet program "
            "into LLVM IR, LLVM bitcode, native assembly, native object code, or an "
            "executable, as well as run the optimized JIT compiled Piet program immediately.");
    args::HelpFlag help(parser, "help", "Display this help menu", {'h', "help"});
    args::ValueFlag<size_t> cs(parser, "size", "Set the codel size manually (0 tries to auto-detect, which usually works, but may be imperfect in certain scenarios)", {"cs", "codel-size"}, 0);
    args::Flag trace(parser, "trace", "Trace program run", {'t', "trace"});
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

        llvm::Type *int_type;
        llvm::Type *number_type;
        llvm::PointerType *stack_type;
        switch(sizeof(long *))
        {
            case 8:
                stack_type = llvm::Type::getInt64PtrTy(context);
                break;
                
            case 4:
                stack_type = llvm::Type::getInt32PtrTy(context);
                break;

            case 2:
                stack_type = llvm::Type::getInt16PtrTy(context);
                break;

            case 1:
                stack_type = llvm::Type::getInt8PtrTy(context);
                break;

            default:
                throw std::runtime_error("Just what architecture are you running, buddy?");
                break;
        }
        switch(sizeof(long))
        {
            case 8:
                number_type = llvm::Type::getInt64Ty(context);
                break;
                
            case 4:
                number_type = llvm::Type::getInt32Ty(context);
                break;

            case 2:
                number_type = llvm::Type::getInt16Ty(context);
                break;

            case 1:
                number_type = llvm::Type::getInt8Ty(context);
                break;

            default:
                throw std::runtime_error("Just what architecture are you running, buddy?");
                break;
        }
        switch(sizeof(int))
        {
            case 8:
                int_type = llvm::Type::getInt64Ty(context);
                break;
                
            case 4:
                int_type = llvm::Type::getInt32Ty(context);
                break;

            case 2:
                int_type = llvm::Type::getInt16Ty(context);
                break;

            case 1:
                int_type = llvm::Type::getInt8Ty(context);
                break;

            default:
                throw std::runtime_error("Just what architecture are you running, buddy?");
                break;
        }

        //printf
        auto printftype = llvm::FunctionType::get(int_type, {llvm::Type::getInt8PtrTy(context)}, true);
        auto printf = llvm::Function::Create(printftype, llvm::Function::ExternalLinkage, "printf", &module);

        //puts
        auto putstype = llvm::FunctionType::get(int_type, {llvm::Type::getInt8PtrTy(context)}, false);
        auto puts = llvm::Function::Create(putstype, llvm::Function::ExternalLinkage, "puts", &module);

        //scanf
        auto scanftype = llvm::FunctionType::get(int_type, {llvm::Type::getInt8PtrTy(context)}, true);
        auto scanf = llvm::Function::Create(scanftype, llvm::Function::ExternalLinkage, "scanf", &module);

        //getchar
        auto getchartype = llvm::FunctionType::get(int_type, false);
        auto getchar = llvm::Function::Create(getchartype, llvm::Function::ExternalLinkage, "getchar", &module);

        //putchar
        auto putchartype = llvm::FunctionType::get(int_type, {int_type}, false);
        auto putchar = llvm::Function::Create(putchartype, llvm::Function::ExternalLinkage, "putchar", &module);

        //realloc.  Hard-aliased pointer types to stack types
        auto realloctype = llvm::FunctionType::get(stack_type, {stack_type, number_type}, false);
        auto realloc = llvm::Function::Create(realloctype, llvm::Function::ExternalLinkage, "realloc", &module);

        //reverse_array
        // array, start, one-past-end
        auto reverse_arraytype = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {stack_type, number_type, number_type}, false);
        auto reverse_array = llvm::Function::Create(reverse_arraytype, llvm::Function::InternalLinkage, "reverse_array", &module);
        {
            auto bb = llvm::BasicBlock::Create(context, "", reverse_array);
            builder.SetInsertPoint(bb);
            std::vector<llvm::Value*> args;
            for (auto &arg: reverse_array->args())
            {
                args.push_back(&arg);
            }
            auto array = args[0];
            array->setName("array");
            auto startarg = args[1];
            startarg->setName("start");
            auto onepastendarg = args[2];
            onepastendarg->setName("onepastend");

#define GETINT llvm::ConstantInt::get

            auto start = builder.CreateAlloca(number_type, 0, "start");
            auto end = builder.CreateAlloca(number_type, 0, "end");
            auto temp = builder.CreateAlloca(number_type, 0, "temp");
            builder.CreateStore(startarg, start);
            builder.CreateStore(builder.CreateSub(onepastendarg, GETINT(number_type, 1)), end);

            auto whileloop = llvm::BasicBlock::Create(context, "while", reverse_array);
            auto whileend = llvm::BasicBlock::Create(context, "whileend", reverse_array);

            auto cond = builder.CreateICmpULT(builder.CreateLoad(start), builder.CreateLoad(end));
            builder.CreateCondBr(cond, whileloop, whileend);
            {
                builder.SetInsertPoint(whileloop);
                builder.CreateStore(builder.CreateLoad(builder.CreateGEP(array, builder.CreateLoad(start))), temp);
                builder.CreateStore(builder.CreateLoad(builder.CreateGEP(array, builder.CreateLoad(end))), builder.CreateGEP(array, builder.CreateLoad(start)));
                builder.CreateStore(builder.CreateLoad(temp), builder.CreateGEP(array, builder.CreateLoad(end)));
                builder.CreateStore(builder.CreateSub(builder.CreateLoad(end), GETINT(number_type, 1)), end);
                builder.CreateStore(builder.CreateAdd(builder.CreateLoad(start), GETINT(number_type, 1)), start);
                cond = builder.CreateICmpULT(builder.CreateLoad(start), builder.CreateLoad(end));
                builder.CreateCondBr(cond, whileloop, whileend);
            }
            {
                builder.SetInsertPoint(whileend);
                builder.CreateRetVoid();
            }
        }

        //rotate_array
        // array, size, depth, amount
        auto rotate_arraytype = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {stack_type, number_type, number_type, number_type}, false);
        auto rotate_array = llvm::Function::Create(rotate_arraytype, llvm::Function::InternalLinkage, "rotate_array", &module);
        {
            auto bb = llvm::BasicBlock::Create(context, "", rotate_array);
            builder.SetInsertPoint(bb);
            std::vector<llvm::Value*> args;
            for (auto &arg: rotate_array->args())
            {
                args.push_back(&arg);
            }
            auto array = args[0];
            array->setName("array");
            auto size = args[1];
            size->setName("arraysize");
            auto depth = args[2];
            depth->setName("depth");
            auto amount = args[3];
            amount->setName("amount");

            auto start = builder.CreateSub(size, depth, "start");
            auto pivot = builder.CreateSub(size, builder.CreateURem(amount, depth), "pivot");
            builder.CreateCall(reverse_array, {array, start, pivot});
            builder.CreateCall(reverse_array, {array, pivot, size});
            builder.CreateCall(reverse_array, {array, start, size});

            builder.CreateRetVoid();
        }

        //popstack
        // stack, pointer to current stack size
        auto popstacktype = llvm::FunctionType::get(number_type, {stack_type, stack_type}, false);
        auto popstack = llvm::Function::Create(popstacktype, llvm::Function::InternalLinkage, "popstack", &module);
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

            auto cond = builder.CreateICmpEQ(GETINT(number_type, 0), builder.CreateLoad(stacksize));
            builder.CreateCondBr(cond, empty, nempty);

            {
                builder.SetInsertPoint(empty);
                builder.CreateRet(GETINT(number_type, 0));
            }
            {
                builder.SetInsertPoint(nempty);
                builder.CreateStore(builder.CreateSub(builder.CreateLoad(stacksize), GETINT(number_type, 1)), stacksize);
                builder.CreateRet(builder.CreateLoad(builder.CreateGEP(stack, builder.CreateLoad(stacksize))));
            }
        }

        ////pushstack
        //// value, stack, pointer to current stack size, pointer to stack reserved size
        auto pushstacktype = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {number_type, llvm::PointerType::get(stack_type, 0), stack_type, stack_type}, false);
        auto pushstack = llvm::Function::Create(pushstacktype, llvm::Function::InternalLinkage, "pushstack", &module);
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
                builder.CreateStore(builder.CreateMul(builder.CreateLoad(reservedsize), GETINT(number_type, 2)), reservedsize);
                llvm::Value *realcall = builder.CreateCall(realloc, {builder.CreateLoad(stack), builder.CreateMul(builder.CreateLoad(reservedsize), GETINT(number_type, (sizeof(size_t))))});
                builder.CreateStore(realcall, stack, false);
                builder.CreateBr(push);
            }
            {
                builder.SetInsertPoint(push);
                builder.CreateStore(value, builder.CreateGEP(builder.CreateLoad(stack), builder.CreateLoad(stacksize)));
                builder.CreateStore(builder.CreateAdd(builder.CreateLoad(stacksize), GETINT(number_type, 1)), stacksize);
                builder.CreateRetVoid();
            }
        }

        auto mainType = llvm::FunctionType::get(int_type, false);
        auto  mainC = module.getOrInsertFunction("main", mainType);
        auto  mainFunc = llvm::dyn_cast<llvm::Function>(mainC);

        auto bb = llvm::BasicBlock::Create(context, "mainblock", mainFunc);
        builder.SetInsertPoint(bb);

        auto dcalloc = builder.CreateAlloca(llvm::Type::getInt8Ty(context), 0, "dc");
        auto i1 = llvm::Type::getInt1Ty(context);
        auto ccalloc = builder.CreateAlloca(i1, 0, "cc");
        auto stackalloc = builder.CreateAlloca(stack_type, 0, "stack");
        auto stacksize = builder.CreateAlloca(number_type, 0, "stacksize");
        auto stackreserved = builder.CreateAlloca(number_type, 0, "stackreserved");
        auto dummyn = builder.CreateAlloca(number_type, 0, "dummyn");
        auto realcall = builder.CreateCall(realloc, {llvm::ConstantPointerNull::get(stack_type), GETINT(number_type, args::get(startstacksize) * sizeof(size_t))});
        builder.CreateStore(realcall, stackalloc, false);
        builder.CreateStore(GETINT(number_type, 0), stacksize, false);
        builder.CreateStore(GETINT(number_type, args::get(startstacksize)), stackreserved, false);
        auto blank = llvm::BasicBlock::Create(context, "blank", mainFunc);
        llvm::ReturnInst::Create(context, GETINT(int_type, 0), blank);




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
            builder.CreateStore(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(startdc)), dcalloc, false);
            builder.CreateStore(GETINT(i1, static_cast<uint8_t>(startcc)), ccalloc, false);

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
                if (trace)
                {
                    std::ostringstream ss;
                    ss << "##########\n"
                        << "Codel   : " << block->codel << '\n'
                        << "Size    : " << block->size << '\n'
                        << "Exits   : \n";
                    for (auto dc: std::list<DC>{DC::North, DC::East, DC::South, DC::West})
                    {
                        for (auto cc: std::list<CC>{CC::Left, CC::Right})
                        {
                            if (!block->neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)].block.expired())
                            {
                                ss << "    " << dc << cc << "  : " << block->exits[static_cast<size_t>(dc)][static_cast<size_t>(cc)] << '\n';
                            }
                        }
                    }
                    builder.CreateCall(puts, {builder.CreateGlobalStringPtr(ss.str())});
                }
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
                    builder.CreateRet(GETINT(int_type, 0));
                    continue;
                }
                auto getNeighbor = [](std::array<std::array<ColorBlock::Neighbor, 2>, 4> &neighbors, DC dc, CC cc)
                {
                    bool toggleCC = true;
                    ColorBlock::Neighbor *neighbor = nullptr;
                    for (neighbor = &neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)];
                            neighbor->block.expired();
                            neighbor = &neighbors[static_cast<size_t>(dc)][static_cast<size_t>(cc)])
                    {
                        if (toggleCC)
                            cc = Toggle(cc);
                        else
                            dc = Toggle(dc);
                        toggleCC = !toggleCC;
                    }
                    return neighbor;
                };

                std::unordered_set<ColorBlock::Neighbor *> neighbors;
                std::map<std::tuple<DC, CC>, ColorBlock::Neighbor *> rneighbors;
                for (auto dc: std::list<DC>{DC::North, DC::East, DC::South, DC::West})
                {
                    for (auto cc: std::list<CC>{CC::Left, CC::Right})
                    {
                        auto neighbor = getNeighbor(block->neighbors, dc, cc);
                        neighbors.emplace(neighbor);
                        rneighbors.emplace(std::tuple<DC, CC>{dc, cc}, neighbor);
                    }
                }
                std::unordered_map<ColorBlock::Neighbor *, llvm::BasicBlock*> neighborbbs;
                for(auto neighbor: neighbors)
                {
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
                lsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::North)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::North, CC::Left})->second)->second);
                lsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::East)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::East, CC::Left})->second)->second);
                lsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::South)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::South, CC::Left})->second)->second);
                lsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::West)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::West, CC::Left})->second)->second);
                builder.SetInsertPoint(right);
                auto rsw = builder.CreateSwitch(builder.CreateLoad(dcalloc), blank, 4);
                rsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::North)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::North, CC::Right})->second)->second);
                rsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::East)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::East, CC::Right})->second)->second);
                rsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::South)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::South, CC::Right})->second)->second);
                rsw->addCase(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(DC::West)), neighborbbs.find(rneighbors.find(std::tuple<DC, CC>{DC::West, CC::Right})->second)->second);
                
                for(auto neighbor: neighbors)
                {
                    auto newdc = neighbor->dc;
                    auto newcc = neighbor->cc;
                    auto bb = neighborbbs.find(neighbor)->second;
                    auto nextbb = blocks.find(neighbor->block.lock())->second;
                    builder.SetInsertPoint(bb);
                    builder.CreateStore(GETINT(llvm::Type::getInt8Ty(context), static_cast<uint8_t>(newdc)), dcalloc);
                    builder.CreateStore(GETINT(i1, static_cast<uint8_t>(newcc)), ccalloc);

                    if (trace)
                    {
                        std::ostringstream ss;
                        ss << "Running operation " << neighbor->operation << " size " << block->size;
                        builder.CreateCall(puts, {builder.CreateGlobalStringPtr(ss.str())});
                    }

                    switch (neighbor->operation)
                    {
                        case OP::PUSH:
                            builder.CreateCall(pushstack, {GETINT(number_type, block->size), stackalloc, stacksize, stackreserved});
                            break;

                        case OP::POP:
                            builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                            break;

                        case OP::ADD:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto first = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto second = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    builder.CreateCall(pushstack, {builder.CreateAdd(second, first), stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::SUBTRACT:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto first = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto second = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    builder.CreateCall(pushstack, {builder.CreateSub(second, first), stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::MULTIPLY:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto first = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto second = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    builder.CreateCall(pushstack, {builder.CreateMul(second, first), stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::DIVIDE:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto first = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto second = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    builder.CreateCall(pushstack, {builder.CreateSDiv(second, first), stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::MOD:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto first = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto second = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto rem = builder.CreateSRem(second, first);
                                    auto neg = llvm::BasicBlock::Create(context, "negative", mainFunc);
                                    auto nonneg = llvm::BasicBlock::Create(context, "nonnegative", mainFunc);
                                    auto cond = builder.CreateICmpSLT(rem, GETINT(number_type, 0));
                                    builder.CreateCondBr(cond, neg, nonneg);
                                    {
                                        builder.SetInsertPoint(neg);
                                        builder.CreateCall(pushstack, {builder.CreateAdd(rem, first), stackalloc, stacksize, stackreserved});
                                        builder.CreateBr(nextbb);
                                    }
                                    {
                                        builder.SetInsertPoint(nonneg);
                                        builder.CreateCall(pushstack, {rem, stackalloc, stacksize, stackreserved});
                                    }
                                }
                            }
                            break;
                        case OP::NOT:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 1));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto value = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto cond = builder.CreateICmpEQ(value, GETINT(number_type, 0));
                                    builder.CreateCall(pushstack, {builder.CreateIntCast(cond, number_type, false), stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::GREATER:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto first = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto second = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto cond = builder.CreateICmpSGT(second, first);
                                    builder.CreateCall(pushstack, {builder.CreateIntCast(cond, number_type, false), stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::POINTER:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 1));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto value = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto rotation = builder.CreateIntCast(builder.CreateSRem(value, GETINT(number_type, 4)), builder.getInt8Ty(), false);
                                    builder.CreateStore(builder.CreateSRem(builder.CreateAdd(builder.CreateLoad(dcalloc), rotation), builder.getInt8(4)), dcalloc);
                                }
                            }
                            break;
                        case OP::SWITCH:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 1));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto value = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto odd = builder.CreateIntCast(builder.CreateSRem(value, GETINT(number_type, 2)), builder.getInt1Ty(), false);
                                    builder.CreateStore(builder.CreateXor(builder.CreateLoad(ccalloc), odd), ccalloc);
                                }
                            }
                            break;
                        case OP::DUPLICATE:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 1));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto value = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    builder.CreateCall(pushstack, {value, stackalloc, stacksize, stackreserved});
                                    builder.CreateCall(pushstack, {value, stackalloc, stacksize, stackreserved});
                                }
                            }
                            break;
                        case OP::ROLL:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 2));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto amount = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    auto depth = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});

                                    // if (depth < 0 || depth > stacksize)
                                    auto baddepth = builder.CreateOr(builder.CreateICmpSLT(depth, GETINT(number_type, 0)), builder.CreateICmpUGT(depth, builder.CreateLoad(stacksize)));
                                    auto badbb = llvm::BasicBlock::Create(context, "baddepth", mainFunc);
                                    auto goodbb = llvm::BasicBlock::Create(context, "gooddepth", mainFunc);
                                    builder.CreateCondBr(baddepth, badbb, goodbb);
                                    {
                                        builder.SetInsertPoint(badbb);
                                        builder.CreateCall(pushstack, {depth, stackalloc, stacksize, stackreserved});
                                        builder.CreateCall(pushstack, {amount, stackalloc, stacksize, stackreserved});
                                        builder.CreateBr(nextbb);
                                    }
                                    {
                                        builder.SetInsertPoint(goodbb);
                                        builder.CreateCall(rotate_array, {builder.CreateLoad(stackalloc), builder.CreateLoad(stacksize), depth, amount});
                                    }
                                }
                            }
                            /*
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
                            }*/
                            break;
                        case OP::INN:
                            {
                                llvm::Value *getn = builder.CreateGlobalStringPtr("%Ld");
                                builder.CreateCall(scanf, {getn, dummyn});
                                builder.CreateCall(pushstack, {builder.CreateLoad(dummyn), stackalloc, stacksize, stackreserved});
                            }
                            break;
                        case OP::INC:
                            {
                                builder.CreateCall(pushstack, { builder.CreateIntCast(builder.CreateCall(getchar), number_type, CHARISSIGNED), stackalloc, stacksize, stackreserved});
                            }
                            break;
                        case OP::OUTN:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 1));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto value = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    llvm::Value *putn = builder.CreateGlobalStringPtr("%Ld");
                                    builder.CreateCall(printf, {putn, value});
                                }
                            }
                            break;
                        case OP::OUTC:
                            {
                                auto iftrue = llvm::BasicBlock::Create(context, "iftrue", mainFunc);
                                auto cond = builder.CreateICmpUGE(builder.CreateLoad(stacksize), GETINT(number_type, 1));
                                builder.CreateCondBr(cond, iftrue, nextbb);
                                {
                                    builder.SetInsertPoint(iftrue);
                                    auto value = builder.CreateCall(popstack, {builder.CreateLoad(stackalloc), stacksize});
                                    builder.CreateCall(putchar, {builder.CreateIntCast(value, int_type, CHARISSIGNED)});
                                }
                            }
                            break;
                        case OP::EXIT:
                            builder.CreateRet(GETINT(number_type, 0));
                            break;
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
