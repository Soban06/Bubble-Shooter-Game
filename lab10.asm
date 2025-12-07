; Muhammad Soban Sohail, 24L-2545

[org 0x100]

jmp start

; Multi-tasking setup
pcb:          times 32*16 dw 0        ; space for 32 PCBs 
stack:        times 32*256 dw 0       ; space for 32 512 byte stacks 
nextpcb:      dw   1                  ; index of next free pcb 
current:      dw   0                  ; index of current pcb
oldtimer: dd 0
BallFallCounter: dw 0
FirstInitDone: db 0

BallRow1Base: dw 204
BallRow2Base: dw 364
BallRow3Base: dw 524

PaddleL: dw 3758
PaddleM: dw 3760
PaddleR: dw 3762

BulletPos: dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
IsBulletActive: dw 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
MaxBullets: dw 10

YouWonTheGameText: db 'Congratulations! You just won the game!', 0
PressEnterToRestartText: db 'Press [Enter] To Restart!', 0

Score: db 0
ScoreText: db 'Score: ', 0

MaxBallCounter: dw 60
CurrentBallsCounter: dw 0

PlayerMovement:
    
 
timer:

    push ds 
    push bx 

    push cs 
    pop  ds                 ; initialize ds to data segment 

    mov  bx, [current]      ; read index of current in bx 
    shl  bx, 1 
    shl  bx, 1 
    shl  bx, 1 
    shl  bx, 1 
    shl  bx, 1              ; multiply by 32 for pcb start 
    mov  [pcb+bx+0], ax     ; save ax in current pcb 
    mov  [pcb+bx+4], cx     ; save cx in current pcb 
    mov  [pcb+bx+6], dx     ; save dx in current pcb 
    mov  [pcb+bx+8], si     ; save si in current pcb 
    mov  [pcb+bx+10], di    ; save di in current pcb 
    mov  [pcb+bx+12], bp    ; save bp in current pcb 
    mov  [pcb+bx+24], es    ; save es in current pcb 

    pop  ax                 ; read original bx from stack 
    mov  [pcb+bx+2], ax     ; save bx in current pcb 
    pop  ax                 ; read original ds from stack 
    mov  [pcb+bx+20], ax    ; save ds in current pcb 
    pop  ax                 ; read original ip from stack 
    mov  [pcb+bx+16], ax    ; save ip in current pcb 
    pop  ax                 ; read original cs from stack 
    mov  [pcb+bx+18], ax    ; save cs in current pcb 
    pop  ax                 ; read original flags from stack 
    mov  [pcb+bx+26], ax    ; save cs in current pcb 
    mov  [pcb+bx+22], ss    ; save ss in current pcb 
    mov  [pcb+bx+14], sp    ; save sp in current pcb 

    mov  bx, [pcb+bx+28]    ; read next pcb of this pcb 
    mov  [current], bx      ; update current to new pcb 
    mov  cl, 5              
    shl  bx, cl             ; multiply by 32 for pcb start 

    mov  cx, [pcb+bx+4]     ; read cx of new process 
    mov  dx, [pcb+bx+6]     ; read dx of new process 
    mov  si, [pcb+bx+8]     ; read si of new process 
    mov  di, [pcb+bx+10]    ; read diof new process

    mov  bp, [pcb+bx+12]    ; read bp of new process 
    mov  es, [pcb+bx+24]    ; read es of new process 
    mov  ss, [pcb+bx+22]    ; read ss of new process 
    mov  sp, [pcb+bx+14]    ; read sp of new process 

    push word [pcb+bx+26]   ; push flags of new process 
    push word [pcb+bx+18]   ; push cs of new process 
    push word [pcb+bx+16]   ; push ip of new process 
    push word [pcb+bx+20]   ; push ds of new process 

    mov  al, 0x20 
    out  0x20, al           ; send EOI to PIC 

    mov  ax, [pcb+bx+0]     ; read ax of new process 
    mov  bx, [pcb+bx+2]     ; read bx of new process 
    pop  ds                 ; read ds of new process 
    iret                    ; return to new process

initpcb:

    push bp 
    mov  bp, sp 

    push ax 
    push bx 
    push cx 
    push si 

    mov  bx, [nextpcb]      ; read next available pcb index 
    cmp  bx, 32             ; are all PCBs used 
    je   exit               ; yes, exit 

    mov  cl, 5               
    shl  bx, cl             ; multiply by 32 for pcb start 

    mov  ax, [bp+8]         ; read segment parameter 
    mov  [pcb+bx+18], ax    ; save in pcb space for cs  
    mov  ax, [bp+6]         ; read offset parameter 
    mov  [pcb+bx+16], ax    ; save in pcb space for ip 

    mov  [pcb+bx+22], ds    ; set stack to our segment 
    mov  si, [nextpcb]      ; read this pcb index 
    mov  cl, 9               
    shl  si, cl             ; multiply by 512  
    add  si, 256*2+stack    ; end of stack for this thread 
    mov  ax, [bp+4]         ; read parameter for subroutine 
    sub  si, 2              ; decrement thread stack pointer 
    mov  [si], ax           ; pushing param on thread stack 
    sub  si, 2              ; space for return address 
    mov  [pcb+bx+14], si    ; save si in pcb space for sp 

    mov  word [pcb+bx+26], 0x0200 ; initialize thread flags 
    mov  ax, [pcb+28]       ; read next of 0th thread in ax 
    mov  [pcb+bx+28], ax    ; set as next of new thread 
    mov  ax, [nextpcb]      ; read new thread index    
    mov  [pcb+28], ax       ; set as next of 0th thread 
    inc  word [nextpcb]     ; this pcb is now used

 
    exit:
        pop  si 
        pop  cx 
        pop  bx 
        pop  ax 
        pop  bp 
        ret  6


PrintScore:

    push bp
    mov bp, sp

    mov di, 326
    mov cx, 12
    mov ah, 0x05
    mov al, '-'
    rep stosw

    mov di, 646
    mov cx, 12
    mov al, '-'
    rep stosw



    mov di, 490
    mov si, ScoreText
    mov ah, 0x07

    l3:
        lodsb
        cmp al, 0
        je printActualScore
        stosw
        jmp l3

    printActualScore:

        mov si, Score

        lodsb
        add al, 0x30
        cmp al, 0x39
        jg greaterThan9
        stosw
        jmp done3

        greaterThan9:
            mov ah, 0
            sub al, 0x30
            xor dx, dx
            mov cx, 10

            div cx      ; dx stores remainder, ax stores quotient
            push dx

            mov ah, 0x07
            add al, 0x30
            stosw
            mov al, dl
            pop ax
            add al, 0x30
            mov ah, 0x07
            stosw



    done3:
        pop bp
        ret

        

ClearScreen:

    push bp
    mov bp, sp

    mov ax, 0xb800
    mov es, ax

    mov ah, 0x07
    mov al, 0x20

    xor di, di

    mov cx, 2000
    rep stosw

    pop bp
    ret


SetupUI:
    push bp
    mov bp, sp

    mov ah, 0x03
    mov al, 0xDC

    mov cx, 40
    xor di, di
    mov di, 40
    rep stosw

    mov cx, 25
    leftColumn:
        jcxz afterLeft
        mov [es:di], ax
        add di, 160
        dec cx
        jmp leftColumn

    afterLeft:
        mov di, 200
        mov cx, 25
    rightColumn:
        jcxz done
        mov [es:di], ax
        add di, 160
        dec cx
        jmp rightColumn

    done:
        call PrintScore
        pop bp
        ret

DrawPaddle:
    push bp
    mov bp, sp

    mov di, [PaddleL]
    mov cx, 3

    mov ah, 0x05
    mov al, 0xDA
    rep stosw

    pop bp
    ret

ErasePaddle:
    push bp
    mov bp, sp

    mov di, [PaddleL]
    mov cx, 3
    mov ah, 0x07
    mov al, 0x20
    rep stosw

    pop bp
    ret

DrawBalls:
    push bp
    mov bp, sp

    mov ax, 0xb800
    mov es, ax

    mov ax, [MaxBallCounter]
    mov word [CurrentBallsCounter], ax
    xor ax, ax

    mov word [BallRow1Base], 204
    mov word [BallRow2Base], 364
    mov word [BallRow3Base], 524

    mov cx, 3
    mov ax, [CurrentBallsCounter]
    xor dx, dx

    div cx      ; ax will store how many balls to generate per row in order to have maximum balls

    push dx

    xor bx, bx
    generateRandom:
        
        cmp bx, ax
        je secondRow
        
        push ax

        mov ah, 0
        int 0x1A

        push cx
        mov cx, 20000
        delay1:
            loop delay1
        pop cx


        mov ax, dx
        xor dx, dx
        mov cx, 37
        
        div cx
        shl dx, 1

        mov di, 204
        add di, dx

        ;shr dx, 1 

        mov cx, 0x074F

        cmp word [es:di], cx
        pop ax
        je generateRandom

        inc bx

        mov word[es:di], 0x074F

        jmp generateRandom

    secondRow:
        xor bx, bx
        secondRowInner:

            cmp bx, ax
            je thirdRow
            
            push ax

            mov ah, 0
            int 0x1A
            
            push cx
            mov cx, 20000
            delay2:
                loop delay2
            pop cx


            mov ax, dx
            xor dx, dx
            mov cx, 37
            
            div cx
            shl dx, 1

            mov di, 364
            add di, dx

            ;shr dx, 1 

            mov cx, 0x074F

            cmp word [es:di], cx
            pop ax
            je secondRowInner

            inc bx

            mov word[es:di], 0x074F

            jmp secondRowInner

    thirdRow:
        xor bx, bx
        pop dx
        add ax, dx
        thirdRowInner:

            cmp bx, ax
            je done4
            
            push ax

            mov ah, 0
            int 0x1A
            
            push cx
            mov cx, 20000
            delay3:
                loop delay3
            pop cx


            mov ax, dx
            xor dx, dx
            mov cx, 37
            
            div cx
            shl dx, 1

            mov di, 524
            add di, dx

            ;shr dx, 1 

            mov cx, 0x074F

            cmp word [es:di], cx
            pop ax
            je thirdRowInner

            inc bx

            mov word[es:di], 0x074F

            jmp thirdRowInner

    done4:
        pop bp
        ret

ShowPressEnterToRestart:
    push bp
    mov bp, sp

    mov ax, 0xb800
    mov es, ax

    xor ax, ax
    mov ah, 0x07

    mov si, PressEnterToRestartText
    mov di, 2134
    l2:
        lodsb
        cmp al, 0
        je doneshow
        stosw
        jmp l2


    doneshow:
        pop bp
        ret

ShowWinningMessage:
    push bp
    mov bp, sp

    xor ax, ax
    mov ah, 0x07

    mov si, YouWonTheGameText
    mov di, 1962
    l1:
        lodsb
        cmp al, 0
        je done2
        stosw
        jmp l1


    done2:
        call ShowPressEnterToRestart
        pop bp
        ret

ClearAllBullets:
    push bp
    mov bp, sp

    mov cx, [MaxBullets]
    mov bx, 0

    ClearLoop:
        mov ax, 2
        mul bx
        mov si, ax

        mov dx, [IsBulletActive + si]
        cmp dx, 1
        jne NextClear

        mov di, [BulletPos + si]
        mov word [es:di], 0x0720

        mov word [IsBulletActive + si], 0

    NextClear:
        inc bx
        loop ClearLoop

        pop bp
        ret

PaddleTask:

    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax
    
PaddleLoop:
    mov ah, 0x01
    int 0x16
    jz PaddleLoop  ; no key pressed, keep checking
    
    mov ah, 0
    int 0x16
    
    cmp ah, 0x4B  ; left arrow
    je PaddleLeft
    
    cmp ah, 0x4D  ; right arrow
    je PaddleRight

    cmp ah, 0x48  ; up arrow to shoot
    je PaddleShoot
    
    jmp PaddleLoop

PaddleShoot:
    mov cx, [MaxBullets]
    mov bx, 0

    FindSlotTask:
        mov ax, 2
        mul bx
        mov si, ax

        mov dx, [IsBulletActive + si]
        cmp dx, 0
        je ActivateBulletTask
        inc bx
        loop FindSlotTask
        jmp PaddleLoop

    ActivateBulletTask:
        mov di, [PaddleM]
        sub di, 160
        mov [BulletPos + si], di

        mov ah, 0x03
        mov al, '|'
        mov [es:di], ax

        mov word [IsBulletActive + si], 1
        jmp PaddleLoop
    
PaddleLeft:
    mov bx, [PaddleL]
    cmp bx, 3722
    jle PaddleLoop
    
    call ErasePaddle
    sub word [PaddleL], 2
    sub word [PaddleM], 2
    sub word [PaddleR], 2
    call DrawPaddle
    jmp PaddleLoop
    
PaddleRight:
    mov bx, [PaddleR]
    cmp bx, 3798
    jge PaddleLoop
    
    call ErasePaddle
    add word [PaddleR], 2
    add word [PaddleM], 2
    add word [PaddleL], 2
    call DrawPaddle
    jmp PaddleLoop

BulletTask:

    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax
    
BulletLoop:
    mov cx, [MaxBullets]
    mov bx, 0
    
BulletCheck:
    mov ax, 2
    mul bx
    mov si, ax
    
    mov dx, [IsBulletActive + si]
    cmp dx, 1
    jne NextBulletTask
    
    mov di, [BulletPos + si]
    mov word [es:di], 0x0720
    sub di, 160
    
    cmp di, 160
    jl DeactivateBulletTask
    
    mov ax, [es:di]
    cmp al, 'O'
    je EraseTargetTask
    
    mov ah, 0x03
    mov al, '|'
    mov [es:di], ax
    mov [BulletPos + si], di
    jmp NextBulletTask
    
EraseTargetTask:
    mov word [es:di], 0x0720
    mov word [IsBulletActive + si], 0
    dec word [CurrentBallsCounter]
    inc byte [Score]
    jmp NextBulletTask
    
DeactivateBulletTask:
    mov word [IsBulletActive + si], 0
    
NextBulletTask:
    inc bx
    loop BulletCheck
    
    ; Small delay
    push cx
    mov cx, 0xFFFF

    delay_bullet:
        loop delay_bullet

    pop cx
    
    jmp BulletLoop


MoveBalls1RowDown:
    ; let's start from the third row
    push bp
    mov bp, sp

    xor ax, ax
    mov ax, 0xb800
    mov es, ax

    mov di, [BallRow3Base]
    mov si, di
    add si, 74

    Checker1:

        cmp di, si
        jge Checker2

        mov ax, [es:di]
        cmp al, 'O'
        je MoveBallDown

        add di, 2
        jmp Checker1

        MoveBallDown:

            mov bx, di
            add bx, 160

            ; Check collision with paddle
            cmp bx, [PaddleL]
            je near BallHitPaddle
            cmp bx, [PaddleM]
            je near BallHitPaddle
            cmp bx, [PaddleR]
            je near BallHitPaddle

            mov word[es:di], 0x0720         ; space
            mov word[es:bx], 0x074F
            add di, 2
            jmp Checker1

    Checker2:
        ; second row
        mov di, [BallRow2Base]
        mov si, di
        add si, 74

        InnerChecker2:

            cmp di, si     ;   again 400 = random
            jge Checker3

            mov ax, [es:di]
            cmp al, 'O'
            je MoveBallDown1

            add di, 2
            jmp InnerChecker2

            MoveBallDown1:
                mov bx, di
                add bx, 160

                ; Check collision with paddle
                cmp bx, [PaddleL]
                je BallHitPaddle
                cmp bx, [PaddleM]
                je BallHitPaddle
                cmp bx, [PaddleR]
                je BallHitPaddle

                mov word[es:di], 0x0720         ; space
                mov word[es:bx], 0x074F
                add di, 2
                jmp InnerChecker2

    Checker3:

        mov di, [BallRow1Base]
        mov si, di
        add si, 74

        InnerChecker3:

            cmp di, si     ;   again 400 = random
            jge done5

            mov ax, [es:di]
            cmp al, 'O'
            je MoveBallDown2

            add di, 2
            jmp InnerChecker3

            MoveBallDown2:
                mov bx, di
                add bx, 160

                ; Check collision with paddle
                cmp bx, [PaddleL]
                je BallHitPaddle
                cmp bx, [PaddleM]
                je BallHitPaddle
                cmp bx, [PaddleR]
                je BallHitPaddle

                mov word[es:di], 0x0720         ; space
                mov word[es:bx], 0x074F

                add di, 2
                jmp InnerChecker3


    done5:
        add word [BallRow1Base], 160
        add word [BallRow2Base], 160
        add word [BallRow3Base], 160

        pop bp
        ret

BallHitPaddle:
    ; Here we handle "loss".
    ; For now, you said: show winning screen to test.

    ; Optional: clear bullets / balls etc.
    call ClearAllBullets

    ; Temporarily reuse winning message function
    call ShowWinningMessage
    call PrintScore

    ; Stop the game loop by jumping to WaitForRestart logic
    ; You already have this in 'start' section:

    ; WaitForRestart:
    ;   mov ah, 0
    ;   int 0x16
    ;   cmp al, 13
    ;   je start
    ;   cmp al, 27
    ;   je Exit
    ;   jmp WaitForRestart

    jmp WaitForRestart  ; make sure this label is global / accessible

ResetGameState:
    ; Critical section: do not let timer + tasks run during reset
    cli

    ; reset vars
    mov word [BallRow1Base], 204
    mov word [BallRow2Base], 364
    mov word [BallRow3Base], 524

    mov word [PaddleL], 3758
    mov word [PaddleM], 3760
    mov word [PaddleR], 3762

    mov byte [Score], 0
    mov word [BallFallCounter], 0
    mov word [CurrentBallsCounter], 0

    call ClearScreen
    call SetupUI

    mov ax, 0xb800
    mov es, ax

    call ClearAllBullets
    call DrawPaddle
    call DrawBalls

    sti       ; re-enable interrupts / scheduler

    ret

SetupInterruptsAndTasks:
    ; Hook timer interrupt and init threads with interrupts OFF
    xor ax, ax
    mov es, ax
    mov ax, [es:8*4]
    mov [oldtimer], ax
    mov ax, [es:8*4+2]
    mov [oldtimer+2], ax

    cli                              ; disable interrupts for whole setup

    mov word [es:8*4], timer
    mov [es:8*4+2], cs

    mov ax, 0xb800
    mov es, ax

    ; Create PaddleTask thread
    push cs
    push word PaddleTask
    push word 0
    call initpcb

    ; Create BulletTask thread
    push cs
    push word BulletTask
    push word 0
    call initpcb

    sti                              ; now everything is ready, enable ints
    ret

start:
    cmp byte [FirstInitDone], 1
    je OnlyResetGame

    ; First time only
    mov byte [FirstInitDone], 1
    call ResetGameState
    call SetupInterruptsAndTasks

    jmp GameLoop

OnlyResetGame:
    call ResetGameState
    jmp GameLoop

GameLoop:
    call PrintScore

    inc word [BallFallCounter]
    cmp word [BallFallCounter], 50
    jb SkipFall

    mov word [BallFallCounter], 0
    call MoveBalls1RowDown

SkipFall:
    mov dx, [CurrentBallsCounter]
    cmp dx, 0
    je ShowWinning
    
    push cx
    mov cx, 0x1FFF
MainDelay:
    loop MainDelay
    pop cx
    
    jmp GameLoop

ShowWinning:
    call ClearAllBullets
    call ShowWinningMessage
    call PrintScore

WaitForRestart:
    mov ah, 0
    int 0x16
    cmp al, 13
    je RestartGame

    cmp al, 27
    je Exit

    jmp WaitForRestart

RestartGame:
    cli
    call ResetGameState
    sti
    jmp GameLoop

Exit:
    ; Restore old timer interrupt
    xor ax, ax
    mov es, ax
    cli
    mov ax, [oldtimer]
    mov [es:8*4], ax
    mov ax, [oldtimer+2]
    mov [es:8*4+2], ax
    sti
    
    mov ax, 0x4c00
    int 0x21