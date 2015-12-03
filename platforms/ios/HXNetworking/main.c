/*
 *  rectangles.c
 *  written by Holmes Futrell
 *  use however you want
 */

#include "HSFFI.h"
#include "lib/Main_stub.h"
#include "SDL/include/SDL_main.h"
int
main (int argc, char *argv[])
{
    hs_init(&argc, &argv);
    haskell_main();
    return 0;
}