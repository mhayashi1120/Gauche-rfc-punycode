/*
 * net_punycode.c
 */

#include "net_punycode.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_net_punycode(void)
{
    return SCM_MAKE_STR("net_punycode is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_net_punycodelib(ScmModule*);

void Scm_Init_net_punycode(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(net_punycode);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("net_punycode", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_net_punycodelib(mod);
}
