.nds
.include "regionSelect.asm"

.open "arm9.bin", 0x02000000

.org US_0x206D578
    ; The first place where the wavi pointer table is accessed.
    ; r1 contains the index, which is originally multiplied by 2
    ; to convert it to the byte offset, but this patch changes
    ; it to multiplying by 4 since each pointer is now 32-bits.
    ; The ldrh is also changed to an ldr instead to read a full word 
    ; instead of half as before.
    stmdb sp!, {lr}
    bl GetPointerHook
    ldmia sp!, {lr}

.org US_0x206D464
    ; As the insertion point is a non-leaf function, code for saving 
    ; the old link register can be omitted.

    ; The second place where the wavi pointer table is accessed,
    ; specifically, after the cmp at 0x206D454, only running if
    ; the pointer is not null. I'm guessing this is related to sample
    ; copying code, as only applying the first patch results in 
    ; samples playing, but playing weirdly.
    bl GetPointerHookWrapper
    nop

.org US_0x206D5B0
    ; The code used to obtain a byte offset for prgi pointer tables 
    ; is pretty much identical to the code for wavi pointer tables,
    ; but the two are still separate functions. This one is for the
    ; prgi tables.
    stmdb sp!, {lr}
    bl GetPointerHookWrapper2
    ldmia sp!, {lr}

.org US_0x2073EE0
    bl ReloadSamples

.org US_0x2073F90
    bl GetRawSampleInfoPointerForIthSlotWrapper

.org US_0x207403C
    bl FixNoteVelocity

.org US_0x20740B0
    bl FixNoteVelocity2

.close

.open "overlay_0036.bin", 0x023A7080
.orga 0x30F70 ; Offset relative to the start of the overlay. This is where the assigned area begins.
; .area 0x48 ; So you don't accidentally overwrite something else
GetRawSampleInfoPointerForIthSlotWrapper:
    stmdb sp!, {r8-r9, lr}

    ldr r8,=Magic

    bl GetRawSampleInfoPointerForIthSlot

    ; Check the note length, and if it is zero, skip all the code for playing the note
    ldr r8,[r9,#0x4] ; Get note length
    cmp r8,0h

    beq skip_note_play
    ; If the note length is not zero, this is just a regular note being played, so continue as usual.
    ldmia sp!, {r8-r9, pc} ; Return back to the original code
skip_note_play:
    ; If it's zero though, skip.
    ldmia sp!, {r8-r9, lr}
    b next_split_loop_start

    .pool
GetPointerHookWrapper:
    ; r7 = index
    ; r2 = pointer table start address
    ; returns r1 = the pointer value
    stmdb sp!, {r0, r2-r5, r7, r9, lr}

    ; Setup the call parameters
    mov r1,r7

    ; Set r9 to null to make sure GetPointerHook doesn't confuse this
    mov r9,0h

    ; Call
    bl GetPointerHook

    ; Get the return value
    mov r1,r0

    ldmia sp!, {r0, r2-r5, r7, r9, pc} ; Return back to the original code
GetPointerHookWrapper2:
    stmdb sp!, {r9, lr}

    ; Set r9 to null to make sure GetPointerHook doesn't confuse this
    mov r9,0h

    ; Call
    bl GetPointerHook

    ldmia sp!, {r9, pc} ; Return back to the original code
GetPointerHook: ; Define a global label so we can use it in the code above
    ; r1 = index
    ; r2 = pointer table start address
    ; returns r0 = the pointer value
    stmdb sp!, {r1-r5, r8, lr}

    ; r0 currently contains the total # of entries in the WAVI table. Keep a copy of that.
    mov r5,r0

    bl PointerSizeCheckBoilerplate

    ; Only run this if the previous check returned equal
    moveq r0,r1, lsl #0x2
    addeq r0,#0x4
    ldreq r0,[r2,r0]

    ; Only run this if the previous check returned not equal
    blne GetPointerNormalHook

    ldr r4,=Magic
    cmp r4,r8
    ; If r8 is not the magic value, then we are not being called from the function that fills out the channel struct, so nothing special needs to be done. Jump to the usual end.
    bne get_pointer_hook_return

    cmp r9,0h
    ; If r9 is null, we are not being called from the function that fills out the channel struct, so nothing special needs to be done. Jump to the usual end.
    beq get_pointer_hook_return

    ; If the code reaches here, we *are* being called from the channel struct filling function, and thus we need to now check if we are being called in a ToggleLoadSamples context.

    ldr r4,[r9,#0x4] ; Get note length
    cmp r4,0h
    ; If the note length is not zero, this is just a regular note being played, so continue as usual.
    bne get_pointer_hook_return

    ; If the code reaches here, this is a ToggleLoadSamples call. Commence the toggling.
    
    cmp r0,0h ; Check if the pointer is currently set.
    movne r4,#0x0 ; If it is set, unset it.
    bleq CalculateSampleInfoOffset  ; moveq r4,something that needs to be calculated ; If it's unset, then set it.

    bl PointerSizeCheckBoilerplate
    bl SetPointer

    ; Since this was a ToggleLoadSamples call, it shouldn't actually play any notes. Put zero into r0 and continue as usual.
    mov r0,0h
    ; b get_pointer_hook_return

get_pointer_hook_return:
    ; Otherwise, continue as usual
    ; This is the original 3rd instruction, which had to be overwritten to preserve the old link register since some of the callers of this code is a leaf function
    cmp r0,0h

    ldmia sp!, {r1-r5, r8, pc} ; Return back to the original code
GetPointerNormalHook:
    mov r0,r1, lsl #0x1
    ldrh r0,[r2,r0]

    bx lr ; Return back to the original code
SetPointer:
    ; Only run this if the previous check returned equal
    moveq r0,r1, lsl #0x2
    addeq r0,#0x4
    streq r4,[r2,r0]

    ; Only run this if the previous check returned not equal
    bne SetPointerNormal

    bx lr ; Return back to the original code
SetPointerNormal:
    ; Only run this if the previous check returned not equal
    mov r0,r1, lsl #0x1
    strh r4,[r2,r0]

    bx lr ; Return back to the original code
PointerSizeCheckBoilerplate:
    ldr r0,[r2] ; Load the first word of the pointer table

    ; Build 0xFFFFFFFF
    mvn r3,0h ; r3 = -1 (0xFFFFFFFF)
    cmp r0,r3

    bx lr ; Return back to the original code
CalculateSampleInfoOffset:
    stmdb sp!, {lr}

    bl PointerSizeCheckBoilerplate

    ; Account for the length of the magic.
    moveq r4,#0x4
    movne r4,#0x0

    ; r5 contains the number of pointer values, and so multiply that by 2 or 4 (extended pointers) to get the total size of the pointer table section.
    addeq r4,r5, lsl #0x2
    addne r4,r5, lsl #0x1

    ; Align to 16 bytes
    orr r4, r4, #0xF
    add r4,1h

    ; r1 contains the index, and so loop through the list of SampleInfo entries until we find the correct one that has an id matching r1.
    add r4,#0x2
sample_info_search_loop:
    ldrh r0,[r2,r4]
    cmp r0,r1
    addne r4,#0x40
    bne sample_info_search_loop
    
    sub r4,#0x2

    ldmia sp!, {pc} ; Return back to the original code
buf0:
    .word 0
buf1:
    .word 0
buf_select:
    .word 0
ReloadSamples:
    stmdb sp!, {lr}
    stmdb sp!, {r0}

    mov r0,0h

    ldrb r0,[r1,#0x2] ; Get note value
    cmp r0,#0x7F
    ; If the note value is not 127, this is just a regular note being played or a ToggleLoadSamples, so continue as usual.
    bne reload_sample_return

    ldrb r0,[r1,#0x3] ; Get note velocity
    cmp r0,#0x7F
    ; If the note velocity is not 127, this is just a regular note being played or a ToggleLoadSamples, so continue as usual.
    bne reload_sample_return

    ; ldr r0,[r1,#0x4] ; Get note length
    ; cmp r0,0h
    ; ; If the note length is not 0, this is just a regular note being played, so continue as usual.
    ; bne reload_sample_return

    ; Otherwise, continue onwards, as this is a ReloadSamples command.

    stmdb sp!, {r1-r12}

    ldr r4,=StructRelatedToPlayback
    ; Load in the bankid and swdlid combination
    ldr r2,[r4,4h]

    ; Setup arguments for calling QueueSamplesForCopyOrJustGetSpaceRequired
    mov r0,r2,lsl 10h
    mov r0,r0,lsr 10h
    mov r1,#0x0
    bl QueueSamplesForCopyOrJustGetSpaceRequired ; r0 now contains the number of bytes required for the state the song is in currently
    
    ; Check if we have already allocated buf0
    ldr r1,=buf0
    ldr r1,[r1]
    mov r2,0h
    cmp r1,r2
    bne buf1_allocation_check_hook ; If it's not equal to null, then it is already allocated, so we can skip ahead.
    ; Otherwise, allocate.
    ; Store the new allocation size into r4+0x30
    str r0,[r4,#0x30]
    ; Allocate a new memory block
    ldr r0,=SoundMemoryArenaPtr
    ldr r0,[r0]
    ldr r1,[r4,#0x30]
    mov r2,#2h
    bl MemLocateSet
    ; r0 now contains the new allocated address. Store in buf0
    ldr r1,=buf0
    str r0,[r1]
    ; str r0,[r4,#0x2c]

buf1_allocation_check_hook:
    ; Check if we have already allocated buf1
    ldr r1,=buf1
    ldr r1,[r1]
    mov r2,0h
    cmp r1,r2
    bne pick_buffer_hook ; If it's not equal to null, then it is already allocated, so we can skip ahead.
    ; Otherwise, allocate.
    ; The allocation size will be the same as buf0, so there's no need to store it in the struct.
    ; Allocate a new memory block
    ldr r0,=SoundMemoryArenaPtr
    ldr r0,[r0]
    ldr r1,[r4,#0x30]
    mov r2,#2h
    bl MemLocateSet
    ; r0 now contains the new allocated address. Store in buf1
    ldr r1,=buf1
    str r0,[r1]

pick_buffer_hook:
    ; Pick which buffer to use based on what's in buf_select
    ldr r0,=buf_select
    ldr r1,[r0]

    mov r2,0h
    cmp r1,r2
    ; If buf_select is 0, use buf0
    ldreq r1,=buf0
    ; If buf_select is 1 (isn't 0), use buf1
    ldrne r1,=buf1
    ; Write
    ldr r1,[r1]
    str r1,[r4,#0x2c]

    ; Update buf_select to use the other buffer next time
    ldr r0,=buf_select
    ldr r1,[r0]
    eor r1,r1,#0x1
    str r1,[r0]

sample_loading_hook:
    bl DataTransferInit

    ldr r0,=StructRelatedToPlayback
    ldr r2,[r0,4h]
    mov r0,r2,lsl 10h
    mov r0,r0,lsr 10h
    bl GetSwdlRepoEntryBySwdlIdandMaybeBankId

    ldr r2,=StructRelatedToPlayback
    ldr r1,[r2,2Ch]
    bl ReadPointerTable

    bl DataTransferStop

    ldmia sp!, {r1-r12}

    ; Added: Rather than continuing with the original code from here, skip everything.
    ldmia sp!, {r0}
    ldmia sp!, {r0} ; Discard return pointer.
    ldmia sp!, {r4,r5,r6,r7,r8,r9,r10,r11,pc}

reload_sample_return:
    ldmia sp!, {r0}
    ldmia sp!, {r9} ; This should contain the address to return to. Since the original code allocates space on the stack, we have to get the value before anything happens to it. r9 is overwritten by the original code, so it's a good place to temporarily store the return address.

    ; Original code
    sub sp,sp,#0xc
    ; mov r10,r0
    ; ldrsh r4,[r10,#0xa]
    ; mov r9,r1

    mov pc,r9 ; Return back to the original code

    .pool
FixNoteVelocity:
    ; Original code
    strb r0,[r4,#0x17]
    ;ldr r0,[DAT_02074144] => since new data is immediately written into r0 afterwards, no need to save what's inside r0

    ldrb r0,[r9,#0x3]
    stmdb sp!, {r0} ; Store it on the stack for later
    mov r0,#0x7F
    strb r0,[r9,#0x3] ; Fix note velocity (note that this needs to be reversed later)

    bx lr ; Return back to the original code
FixNoteVelocity2:
    ; Original code
    ldrb r2,[r10,#0x59]
    ;ldrsh r3,[r10,#0x52]
    ;add r0,r4,#0x5c => again, since r0 is immediately overwritten, no need to save its value.

    ldmia sp!, {r0} ; Get the old velocity back from the stack
    strb r0,[r9,#0x3]

    bx lr ; Return back to the original code
; .endarea
.close