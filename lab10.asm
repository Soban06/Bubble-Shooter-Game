; Muhammad Soban Sohail, 24L-2545

[org 0x100]

jmp start

; Multi-tasking setup
pcb:          times 32*16 dw 0        ; space for 32 PCBs 
stack:        times 32*256 dw 0       ; space for 32 512 byte stacks 
nextpcb:      dw   1                  ; index of next free pcb 
current:      dw   0                  ; index of current pcb 

BallFallCounter: dw 0

welcomeTo: db 'WELCOME TO', 0
pongText: db 'KNOX', 0
pressEnterToContinueText: db 'PRESS [ENTER] TO CONTINUE', 0

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
YouLostTheGameText: db 'You just lost the game :(', 0
PressEnterToRestartText: db 'Press [Enter] To Restart!', 0

Score: db 0
ScoreText: db 'Score: ', 0

MaxBallCounter: dw 20
CurrentBallsCounter: dw 0

PrintSideArrows:
    push bp
    mov bp, sp

    mov ah, 0x03
    mov al, 'X'

    mov cx, 80
    
    xor di, di

    rep stosw

    mov cx, 23
    leftColumn2:
        jcxz bottom2
        mov [es:di], ax
        add di, 160
        dec cx
        jmp leftColumn2

    bottom2:
        mov cx, 79
        rep stosw

    mov cx, 25
    rightColumn2:
        jcxz done8
        mov [es:di], ax
        sub di, 160
        dec cx
        jmp rightColumn2


    done8:
        pop bp
        ret

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


PrintWelcomeScreen:
    push bp
    mov bp, sp

    call PrintSideArrows

    mov ah, 0x03

    mov di, 1346
    mov si, welcomeTo


    mov cx, 10
    PrintWelcomeTo:
        lodsb
        stosw
        dec cx
        jcxz PONG
        jmp PrintWelcomeTo

    PONG:

        mov di, 1672
        mov si, pongText

        mov cx, 4

        printPong:
            lodsb
            stosw
            dec cx
            jcxz pressEnter
            jmp printPong


    pressEnter:
        mov di, 2292
        mov si, pressEnterToContinueText

        mov cx, 25
        mov ah, 0x03
        printEnterToCont:
            lodsb
            stosw
            dec cx
            jcxz done7
            jmp printEnterToCont

    done7:
        pop bp
        ret


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
        jcxz bottom
        mov [es:di], ax
        add di, 160
        dec cx
        jmp rightColumn

    bottom:

        mov di, 3882
        mov cx, 39
        mov ah, 0x0C
        mov al, 0xDC
        bottomloop:
            jcxz done
            mov [es:di], ax
            add di, 2
            dec cx
            jmp bottomloop

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

    mov ax, [MaxBallCounter]
    mov word [CurrentBallsCounter], ax
    xor ax, ax

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

        mov ch, 0x07
        mov cl, 'O'

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

            mov ch, 0x07
            mov cl, 'O'

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

            mov ch, 0x07
            mov cl, 'O'

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
    mov di, 2136
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

ShowLosingMessage:
    push bp
    mov bp, sp

    xor ax, ax
    mov ah, 0x0C

    mov si, YouLostTheGameText
    mov di, 1816
    l4:
        lodsb
        cmp al, 0
        je done6
        stosw
        jmp l4


    done6:
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
            
            mov word[es:di], 0x0720         ; space
            mov word[es:bx], 0x074F

            ; Check collision with paddle
            cmp bx, [PaddleL]
            je near BallHitPaddle
            cmp bx, [PaddleM]
            je near BallHitPaddle
            cmp bx, [PaddleR]
            je near BallHitPaddle

            xor dx, dx
            mov ax, di
            mov cx, 160
            div cx
            cmp ax, 23
            je near BallHitEnd

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

                mov word[es:di], 0x0720         ; space
                mov word[es:bx], 0x074F

                ; Check collision with paddle
                cmp bx, [PaddleL]
                je near BallHitPaddle
                cmp bx, [PaddleM]
                je near BallHitPaddle
                cmp bx, [PaddleR]
                je near BallHitPaddle

                xor dx, dx
                mov ax, di
                mov cx, 160
                div cx
                cmp ax, 23
                je near BallHitEnd

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

                mov word[es:di], 0x0720         ; space
                mov word[es:bx], 0x074F

                ; Check collision with paddle
                cmp bx, [PaddleL]
                je near BallHitPaddle
                cmp bx, [PaddleM]
                je near BallHitPaddle
                cmp bx, [PaddleR]
                je near BallHitPaddle

                xor dx, dx
                mov ax, di
                mov cx, 160
                div cx
                cmp ax, 23
                je near BallHitEnd

                add di, 2
                jmp InnerChecker3


    done5:
        add word [BallRow1Base], 160
        add word [BallRow2Base], 160
        add word [BallRow3Base], 160

        pop bp
        ret

BallHitPaddle:
    call ClearAllBullets
    call PrintScore

    jmp ShowLosing

BallHitEnd:
    call ClearAllBullets
    call PrintScore

    jmp ShowLosing

start:

    mov ax, 0xb800
    mov es, ax

    call ClearScreen

    call PrintWelcomeScreen

    WaitForEnter:
        mov ah, 0
        int 0x16

        cmp al, 13
        jne WaitForEnter

    mov word [PaddleL], 3758
    mov word [PaddleM], 3760
    mov word [PaddleR], 3762

    mov word [BallRow1Base], 204
    mov word [BallRow2Base], 364
    mov word [BallRow3Base], 524

    mov word [BallFallCounter], 0
    mov word [CurrentBallsCounter], 0
    mov byte [Score], 0

    call ClearScreen
    call SetupUI
    call DrawPaddle
    call DrawBalls

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
        je near ShowWinning
        
        push cx
        mov cx, 0x1FFF

        mov cx, [MaxBullets]
        mov bx, 0

        HandleBullets:
            mov ax, 2
            mul bx
            mov si, ax

            mov dx, [IsBulletActive + si]
            cmp dx, 1
            je SimulateBullet
            jmp NextBullet

        SimulateBullet:

            mov di, [BulletPos + si]

            mov word [es:di], 0x0720
            sub di, 160

            cmp di, 160
            jl DeactivateBullet
            mov ax, [es:di]
            
            cmp al, 'O'
            je EraseTarget

            mov ah, 0x03
            mov al, '|'

            mov [es:di], ax


            mov [BulletPos + si], di
            jmp NextBullet
            

            EraseTarget:
                mov word [es:di], 0x0720
                mov word[IsBulletActive + si], 0

                dec word [CurrentBallsCounter]
                mov dx, [CurrentBallsCounter]
                cmp dx, 0

                inc byte[Score]

                cmp dx, 0
                je CheckWin
                jmp DeactivateBullet

                CheckWin:
                    jmp ShowWinning

            DeactivateBullet:

                mov word [IsBulletActive + si], 0


            NextBullet:
                inc bx
                loop HandleBullets
        
        

        Continue:

            mov cx, 0xAAAA
            delay_loop:
                loop delay_loop

            mov ah, 0x01
            int 0x16
            jz GameLoop

            mov ah, 0
            int 0x16

            cmp al, 27
            je near Exit

            cmp ah, 0x4B
            je MoveLeft

            cmp ah, 0x4D
            je MoveRight

            cmp ah, 0x48
            je Shoot

            jmp GameLoop

            MoveLeft:
                mov bx, [PaddleL]
                cmp bx, 3722
                jle GameLoop

                call ErasePaddle

                sub word [PaddleL], 2
                sub word [PaddleM], 2
                sub word [PaddleR], 2

                call DrawPaddle
                jmp GameLoop

            MoveRight:
                mov bx, [PaddleR]
                cmp bx, 3798
                jge GameLoop

                call ErasePaddle

                add word [PaddleR], 2
                add word [PaddleM], 2
                add word [PaddleL], 2
                
                call DrawPaddle
                jmp GameLoop

            Shoot:

                mov cx, [MaxBullets]
                mov bx, 0

                FindSlot:
                    mov ax, 2
                    mul bx
                    mov si, bx

                    mov dx, [IsBulletActive + si]
                    cmp dx, 0
                    je ActivateBullet
                    inc bx
                    loop FindSlot

                    jmp GameLoop

                ActivateBullet:

                    mov ax, 2
                    mul bx
                    mov si, ax

                    mov di, [PaddleM]
                    sub di, 160
                    mov [BulletPos + si], di

                    mov ah, 0x03
                    mov al, '|'
                    mov [es:di], ax

                    mov word [IsBulletActive + si], 1
                    jmp GameLoop               

    
    ShowWinning:
        call ClearAllBullets
        call ShowWinningMessage
        call PrintScore

        WaitForRestart:
            mov ah, 0
            int 0x16
            cmp al, 13
            je start

            cmp al, 27
            je Exit

            jmp WaitForRestart

    ShowLosing:
        call ClearAllBullets
        call ShowLosingMessage
        call PrintScore

        WaitForRestart2:
            mov ah, 0
            int 0x16
            cmp al, 13
            je start

            cmp al, 27
            je Exit

            jmp WaitForRestart2

    Exit:
        mov ax, 0x4c00
        int 0x21