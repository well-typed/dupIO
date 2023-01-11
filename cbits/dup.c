#include "Rts.h"

void dupUnsupportedWarning(StgClosure *closure) {
    IF_DEBUG(sanity, fprintf(stderr,"Closure of non-dupable type %d passed to dup!\n", get_itbl(closure)->type));
}

