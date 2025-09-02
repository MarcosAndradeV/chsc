#include <string.h>
#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#define NOB_EXPERIMENTAL_DELETE_OLD
#include "nob.h"
#define try(expr) if(!expr) return false
static Cmd cmd = {0};

int main(int argc, char **argv)
{
    NOB_GO_REBUILD_URSELF(argc, argv);

    if(argc >= 1) (void)shift(argv, argc); // discart program name

    try(mkdir_if_not_exists("build"));

    nob_cc(&cmd);
    nob_cc_flags(&cmd);
    nob_cc_inputs(&cmd, "src/main.c");
    nob_cc_output(&cmd, "build/chsc");
    cmd_append(&cmd, "-ggdb");
    try(cmd_run(&cmd));

    if(argc >= 1 && strcmp(shift(argv, argc), "run") == 0) {
        cmd_append(&cmd, "build/chsc");
        if(argc >= 1 && strcmp(shift(argv, argc), "--") == 0) {
            da_append_many(&cmd, argv, argc);
        }
        try(cmd_run(&cmd));
    }

    return 0;
}
