#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType)

static ObjString *allocateString(char *chars, int length, uint32_t hash) {
  printf("allocateString\n");
  ObjString *string = ALLOCATE(ObjString, 1);
  string->length = length;
  string->chars = chars;
  string->hash = hash;

  printf("interning string\n");
  // intern the string we allocated
  tableSet(&vm.strings, string, NIL_VAL);

  printf("end allocation\n");
  return string;
}

static uint32_t hashString(const char *key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

ObjString *copyString(const char *chars, int length) {
  printf("copyString\n");
  uint32_t hash = hashString(chars, length);

  // if we already have this string interned, return it so it's the same value
  // in memory
  ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    return interned;
  }

  char *heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';
  printf("begin allocation\n");
  return allocateString(heapChars, length, hash);
}

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;
  object->next = vm.objects;
  vm.objects = object;
  return object;
}

ObjString *takeString(char *chars, int length) {
  uint32_t hash = hashString(chars, length);

  // hand over ownership of the string to the VM
  ObjString *interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }

  return allocateString(chars, length, hash);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}