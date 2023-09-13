#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

void writeConstant(Chunk* chunk, Value value);

void writeConstant(Chunk* chunk, Value value) {
    int constant = addConstant(chunk, value);
    writeChunk(chunk, OP_CONSTANT, 123);
    writeChunk(chunk, constant, 123);
}

int main(int argc, const char *argv[]) {
    initVM();

    Chunk chunk;
    initChunk(&chunk);

    writeConstant(&chunk, 1);
    writeConstant(&chunk, 3);
    writeChunk(&chunk, OP_ADD, 123);
    writeConstant(&chunk, 4);

    writeChunk(&chunk, OP_DIVIDE, 123);
    writeChunk(&chunk, OP_NEGATE, 123);
    writeChunk(&chunk, OP_RETURN, 123);
    interpret(&chunk);

    freeVM();
    freeChunk(&chunk);
    return 0;
}