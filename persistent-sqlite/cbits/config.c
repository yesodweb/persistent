/* This file defines auxiliary functions to help dealing with sqlite vararg stuff. */
#include <sqlite3.h>

int persistent_sqlite_set_log(void (*logFn)(void*, int, const char*), void* arg) {
  return sqlite3_config(SQLITE_CONFIG_LOG, logFn, arg);
}

long long int persistent_sqlite3_soft_heap_limit64(long long int N) {
  return sqlite3_soft_heap_limit64(N);
}
