/*
** $Id: cthreadlib.c $
** Standard Thread library
*/

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/threads.h>

typedef struct {
    value closure;
} thread_arg_t;

void* thread_start(void* arg) {
    thread_arg_t* t = (thread_arg_t*)arg;

    caml_acquire_runtime_system();
    caml_callback_exn(t->closure, Val_unit);
    caml_release_runtime_system();

    caml_remove_global_root(&t->closure);
    free(t);
    return NULL;
}

CAMLprim value spawn_thread(value closure) {
    CAMLparam1(closure);
    pthread_t tid;
    thread_arg_t* arg = (thread_arg_t*)malloc(sizeof(thread_arg_t));
    arg->closure = closure;
    caml_register_global_root(&arg->closure);

    pthread_create(&tid, NULL, thread_start, arg);
    pthread_detach(tid);
    CAMLreturn(Val_unit);
}