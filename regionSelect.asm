_REGION equ "US"

.relativeinclude on

; The equs inside the offset files can't be inside a conditional directive so we have to do this instead
.include "offsets" + _REGION + ".asm"

.relativeinclude off