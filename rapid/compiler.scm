(define (emit-object-code assembly)
  (write-string "
/* A Rapid Scheme object file, made by Rapid Scheme 0.1.  */

#include <lightning.h>
#include <stdio.h>

static jit_state_t *_jit;

int
main (int argc, char *argv[])
{
  init_jit (argv[0]);
  _jit = jit_new_state ();

  jit_prolog ();
  jit_frame (256);
  jit_pushargi ((jit_word_t) \"Hello, World!\\n\");
  jit_finishi (printf);
  jit_ret ();
  jit_epilog ();

  void (*function) (void) = jit_emit ();
  jit_clear_state ();

  function ();
  
  jit_destroy_state ();
  finish_jit ();
}
"))
