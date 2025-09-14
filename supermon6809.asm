;V10 Help routine improved with dynamic subroutine adresses
;V9     Fill command added Fill 2000 3000 ff
;V8     COPY command added. this command copes a memory block 
;        to other location.
;V7    
;       Stabile version
;       "SAVE" command added
;v6;
;       Command parser improved
;       Command table addded
;       minor improvements
;supermon-modular-5.asm;
;    -Stabile version
;    -Parser Kernel Separated
;    -flexible command input: r,run,dump,d...
;    -flexible addr input: DUMP 0 120 or D 250 1FFF
;    -many small bugs fixed
;    -ready to add small interpreters
;    -this is final version of Supermon's first stage
; SuperMon-6809 — FINAL (Parser Kernel + Exec Routines)
; Goal: Keep behavior/visuals identical; refactor into KERNEL (parse) + EXEC (run)
; Extra: addresses parsed "as-is" (1..4 hex nibbles, left-padded to 16-bit).
; Rules: No '.' and '' in labels or variables.

               

; ===================== Direct Page Variables =====================
addrhi          equ   $40        ; active address HI
addrlo          equ   $41        ; active address LO
RECLEN          equ   $42        ; loader length
TMP             equ   $44        ; temp
HEXLDREG        equ   $45        ; generic counter
PTR             equ   $46        ; DP pointer MSB
PTRL            equ   $47        ; DP pointer LSB (PTR+1)

LOADSTA         equ   $52        ; generic 16-bit (used as END for dump)
LOADSTAL        equ   $53

TMPH            equ   $57        ; temp hi
TMPL            equ   $58        ; temp lo

; --- Parser/Command kernel DP ---
INLEN           equ   $60        ; line length
INPOS           equ   $61        ; parse cursor (0..INLEN)
TOK             equ   $62        ; temp char

CMDID           equ   $64        ; command id (R=1,D=2,M=3,L=4,H=5)
ARGW0HI         equ   $65        ; first 16-bit argument (e.g., start addr)
ARGW0LO         equ   $66
ARGW1HI         equ   $67        ; second 16-bit argument (e.g., end addr)
ARGW1LO         equ   $68
ARGCNT          equ   $69        ; used as nibble counter in READADDR
ADLEN0          equ   $6A       ; 1. adresin nibble sayısı (1..4)
ADLEN1          equ   $6B       ; 2. adresin nibble sayısı (1..4)

SAVECHKS       equ   $6C       ; kayit checksum toplami (low byte tutulacak)
SAVECNT        equ   $6D       ; satir icin kalem uzunlugu sayaci (0..16)

ARGW2HI        equ   $6E       ; ucuncu 16-bit arguman (len)
ARGW2LO        equ   $6F
; --- Line buffer ---
INBUF           equ   $80        ; 128-byte input line buffer ($80..$FF)

; ===================== ACIA 6850 =====================
aciactl         equ   $A000
aciasta         equ   $A000
aciadat         equ   $A001

; ===================== Reset / Greet =====================

ramstart    equ    $1FFF
ldustart    equ     $1BFF

; ========== Giriş Noktası ==========
; -------------------------------
; SYSTEM INIT: Stack'ları kurar
; -------------------------------
                ORG     $C000

;INITSYSTEM:
reset:     
            lds  #ramstart    ; Stack init
            ldU  #ldustart     
            SEI                 ; (opsiyonel) disable interrupts
            CLRA
            TFR   A,DP          ; DP = $00  --> direct page 00 sayfası            
            jsr initacia

GREET:
        ldx  #GMSG              ; greeting
        jsr  PRINT              ; print
        jmp  MON                ; go monitor

; ===================== Bootloader (UNCHANGED behavior) =====================
LOADERZ:
        ldx  #LMSG              ; loader msg
        jsr  PRINT              ; print
        jmp  INTLIN1            ; to loader

LOADED:
        ldx  #DONEMSG           ; "Start Address: $"
        jsr  PRINT
        lda  LOADSTA            ; start HI
        jsr  HEXOUT
        lda  LOADSTAL           ; start LO
        jsr  HEXOUT
        lda  #$0D               ; CR
        jsr  OUTCH
        lda  #$0A               ; LF
        jsr  OUTCH
        ldx  LOADSTA            ; X=start
        lbra MON                ; back to monitor

INTLIN1:
        clra
        sta  HEXLDREG
INTLIN:
        jsr  GETCH
        cmpa #':'
        bne  INTLIN
        jsr  BYTERD             ; len
        sta  RECLEN
        jsr  BYTERD             ; addr HI
        sta  addrhi
        jsr  BYTERD             ; addr LO
        sta  addrlo
        ldx  addrhi
        lda  HEXLDREG
        cmpa #$01
        beq  GOON
        lda  #$01
        sta  HEXLDREG
        stx  LOADSTA
GOON:   jsr  BYTERD             ; rectype
        cmpa #$01               ; EOF?
        beq  GETGARBAGE
DATAIN: jsr  BYTERD
        sta  ,x
        leax 1,x
        dec  RECLEN
        lda  RECLEN
        bne  DATAIN
        bra  INTLIN
GETGARBAGE:
        jsr  GETCH             ;must be replaced non polling read ch!!
        jsr  GETCH
        jsr  GETCH
        jmp  LOADED

BYTERD:                         ; read 2 hex chars -> byte (A)
        clr  TMPH
BYTH:   jsr  GETCH
        jsr  HEXCON
        bcs  BYTH
        asla
        asla
        asla
        asla
        anda #$F0
        sta  TMPH
        clr  TMPL
BYTL:   jsr  GETCH
        jsr  HEXCON
        bcs  BYTL
        anda #$0F
        sta  TMPL
        lda  TMPL
        ora  TMPH
        sta  TMP
        rts

HEXCON:                         ; ASCII '0'..'9','A'..'F','a'..'f' -> nibble in A, C=0 ok, C=1 err
        cmpa #'0'
        blo  HEXBAD
        cmpa #'9'
        bhi  HEXALPHA
        suba #'0'
        clc
        rts
HEXALPHA:
        cmpa #'A'
        blo  HEXLOW
        cmpa #'F'
        bhi  HEXLOW
        suba #'A'
        adda #10
        clc
        rts
HEXLOW:
        cmpa #'a'
        blo  HEXBAD
        cmpa #'f'
        bhi  HEXBAD
        suba #'a'
        adda #10
        clc
        rts
HEXBAD: clra
        sec
        rts

HEXOUT:                         ; print A as two hex chars
        pshs x,b,a
        tfr  a,b
        anda #$F0
        lsra
        lsra
        lsra
        lsra
        sta  TMPH
        jsr  HEXAS
        tfr  b,a
        anda #$0F
        sta  TMPL
        jsr  HEXAS
        puls a,b,x
        rts

HEXAS:
        pshs x,a
        ldx  #TABLEX
        leax a,x
        lda  ,x
        jsr  OUTCH
        puls a,x
        rts

PRINT:                          ; print 0-terminated string at X
        lda  ,x
        beq  PEND
        jsr  OUTCH
        leax 1,x
        bra  PRINT
PEND:   rts

initacia:
        lda  #$03               ; master reset
        sta  aciactl
        lda  #%00010101         ; 8N1, /16
        sta  aciactl
        rts

GETCH:                          ; read char, force A..Z
        lda  aciasta
        bita #$01
        beq  GETCH
        lda  aciadat
        cmpa #'a'
        blo  gsup
        cmpa #'z'
        bhi  gsup
        anda #$DF               ; to upper
gsup:   rts

OUTCH:                          ; write char in A
        pshs a
po1:    lda  aciasta
        bita #$02
        beq  po1
        puls a
        sta  aciadat
        rts

; ===================== Messages =====================
GMSG:   fcb $0c,$07
        fcc "SuperMon 6809 By Dr. Peker 2025"
        fcb $0a,$0d
        fcc "Version: modular-10, @ $C000"
        fcb $0a,$0d,$0a,$0d,$0a,$0d
        
        fcc " LOAD  - intel Hex loader"
        fcb $0a,$0d,$0a,$0d
        fcc " SAVE  - intel Hex Save"
        fcb $0a,$0d,$0a,$0d
        fcc " DUMP  - dump memory content"
        fcb $0a,$0d,$0a,$0d
        fcc " MEM   - Memory  edit"
        fcb $0a,$0d,$0a,$0d
        fcc " COPY  - Block copy in memory"
        fcb $0a,$0d,$0a,$0d
        fcc " FILL  - Fill in memory"
        fcb $0a,$0d,$0a,$0d
        fcc " RUN   - Run a program"
        fcb $0a,$0d,$0a,$0d
        
        fcc " HELP  - Help"
        fcb $0a,$0d,$0a,$0d
        fcc "Ready."
        fcb $0a,$0d,$00

LMSG:   fcb $0a,$0d
        fcc "Send iHex file from your terminal!"
        fcb $0a,$0d
        fcc "Loading..."
        fcb $00

DONEMSG:fcb $0a,$0d
        fcc "Start Address: $"
        fcb $00

TABLEX: fcc "0123456789ABCDEF"

HLPMSG: fcb $0c
        fcc "   LOAD   :Starts intel Hex loader"
        fcb $0a,$0d,$0a,$0d
        fcc "   SAVE addr addr lenght:    intel Hex save"
        fcb $0a,$0d,$0a,$0d
        fcc "   DUMP addr addr : dumps between two adresses "
        fcb $0a,$0d,$0a,$0d
        fcc "   MEM addr aa bb cc dd... <CR> "
        fcb $0a,$0d
        fcc "       edits specified memory address-es"
        fcb $0a,$0d,$0a,$0d
        fcc "   COPY source dest lenght"
        fcb $0a,$0d,$0a,$0d
        fcc "   FILL addr addr val"
        fcb $0a,$0d,$0a,$0d
        fcc "   RUN addr : Runs program at given address"
        fcb $0a,$0d,$0a,$0d
         
        fcc "Note: in monitor every address area can be used  "
        fcb $0a,$0d
        fcc "      as A AA AAA or AAAA"
        fcb $0a,$0d,$0a,$0d
       
        fcc "   Useful Subroutines (call with jsr EXCEPT prompt):"
        fcb $0a,$0d,$0a,$0d
        fcb $00

;Help shows Useful routine adresses Compile adress independently;
GETCHMSG:        
        fcc "     GETCH  : $"   
        fcb $00
OUTCHMSG:        
        fcc "     OUTCH  : $"
        fcb $00
PRINTXMSG:        
        fcc "     PRINT X: $"   
        fcb $00
PRINTAMSG:        
        fcc "     PRINT A: $"
        fcb $00
DELAYMSG:        
        fcc "     DELAY  : $" 
        fcb $00
SPACEMSG:   
        fcc "      "
        fcb $00
PROMPTMSG:   
        fcc "     PROMT  : $"
        fcb $00 
HEXOUTMSG:   
        fcc "     HEXOUT : $"
        fcb $00                 



PMON:   fcc "S:>"
        fcb $00

PHELP:  fcc "?"
        fcb $00
;-------------------Command Keywords----------------------

; --- Command keywords (full words only) ---
KWDUMP:   fcc "DUMP"   
            fcb $0
KWMEMORY: fcc "MEMORY" 
            fcb $0
KWMEM:    fcc "MEM"    
            fcb $0
KWLOAD:   fcc "LOAD"   
            fcb $0
KWHELP:   fcc "HELP"   
            fcb $0
KWRUN:    fcc "RUN"    
            fcb $0

KWSAVE:   fcc "SAVE"   
            fcb $0
KWCOPY:   fcc "COPY"   
            fcb $0                        
KWFILL:   fcc "FILL"   
            fcb $0  

;--------------------Command Keywords ended---------------
; ===================== Small I/O helpers =====================
CRLF:
        lda  #$0D
        jsr  OUTCH
        lda  #$0A
        jsr  OUTCH
        rts

PUTSP:
        lda  #' '
        jsr  OUTCH
        rts

; ===================== Line Buffer (unchanged) =====================
READLINE:                       ; read into INBUF with BS/DEL, echo
        clr  INLEN
RLLOOP:
        jsr  GETCH
        cmpa #$0D
        beq  RLDONE
        cmpa #$08
        beq  RLBS
        cmpa #$7F
        beq  RLBS
        cmpa #' '
        blo  RLLOOP
        ldb  INLEN
        cmpb #127
        bhs  RLBELL
        ldx  #INBUF
        leax b,x
        sta  ,x
        jsr  OUTCH
        inc  INLEN
        bra  RLLOOP
RLBS:
        ldb  INLEN
        beq  RLLOOP
        dec  INLEN
        lda  #$08
        jsr  OUTCH
        lda  #' '
        jsr  OUTCH
        lda  #$08
        jsr  OUTCH
        bra  RLLOOP
RLBELL:
        lda  #$07
        jsr  OUTCH
        bra  RLLOOP
RLDONE:
        jsr  CRLF
        ldb  INLEN
        ldx  #INBUF
        leax b,x
        clr  ,x
        rts
;----------------MATCH COMMAND WORD------------------------------


SINIT:                          ; parser init
        clr  INPOS
        rts

SPEEK:                          ; A= current char or 0
        ldb  INPOS
        cmpb INLEN
        bhs  SPEEKEND
        ldx  #INBUF
        leax b,x
        lda  ,x
        rts
SPEEKEND:
        clra
        rts

SADV:                           ; advance cursor
        ldb  INPOS
        cmpb INLEN
        bhs  SADVRET
        inc  INPOS
SADVRET:
        rts

SKIPSP:                         ; skip space/comma/$
SSKIP:  jsr  SPEEK
        beq  SSKIPEND
        cmpa #' '
        beq  SEAT
        cmpa #','
        beq  SEAT
        cmpa #'$'
        beq  SEAT
        rts
SEAT:   jsr  SADV
        bra  SSKIP
SSKIPEND:
        rts

; MATCHWORD: INPOS'tan itibaren Y=pattern("TXT",0) ile TAM eslesmeyi dener.
; Tam eslesirse: C=0, INPOS kelimenin sonrasina tasinir.
; Degilse: C=1 ve INPOS degismez.
MATCHWORD:
        pshs  d,x,y
        ldb   INPOS
        ldx   #INBUF
        leax  b,x              ; X = INBUF + INPOS
        ; COMPARE PATTERN BY LINE
        ; pattern'i satirla karsilastir
MWLOOP:
        lda   ,y               ; pattern bayti
        beq   MWENDPAT         ; 0 -> bitti
        cmpa  ,x
        bne   MWFAIL
        leax  1,x
        leay  1,y
        bra   MWLOOP

MWENDPAT:                      ; pattern bitti -> sonraki karakter harf mi?
        lda   ,x               ; sonraki karakter
        cmpa  #'A'
        blo   MWOK             ; harf degil → TAM
        cmpa  #'Z'
        bhi   MWOK             ; harf degil → TAM
        bra   MWFAIL           ; harfse kismi eslesme = basarisiz

MWOK:
        ; INPOS = X - INBUF   (INPOS 1 bayt, sadece dusuk bayti yaz)
        pshs  x
        ldx   #INBUF
        stx   TMP              ; TMP = INBUF (MSB onemsiz)
        puls  x                ; X = eslesen kelimenin SONRASI
        tfr   x,d              ; D = X
        subd  TMP              ; D = X - INBUF
        stb   INPOS            ; yalnizca dusuk bayti sakla
        puls  d,x,y
        clc
        rts

MWFAIL:
        puls  d,x,y
        sec
        rts

; ===================== NEW Parser Kernel =====================

; READCMD  -> extracts command id into CMDID (first letter R/D/M/L/H)
; READADDR -> reads 1..4 hex nibbles as 16-bit, left-padded, echoing hex chars
; PARSEARGS-> fills ARGW0/ARGW1 depending on CMDID

; READCMD: komutu tanır ve komut KELİMESİNİ tüketir (A..Z), INPOS argümanın başında kalır
; READCMD: Yalnizca TAM komut kelimesi eslesirse kabul eder.
; Destek: DUMP, MEM, MEMORY, LOAD, HELP, RUN
; Tek harf YOK (istersen tabloya ayri kelime olarak eklersin).
READCMD:
        jsr  SKIPSP             ; ayiraclari atla
        jsr  SPEEK              ; A = ilk karakter
        tsta
        beq  RCERR              ; satir bos

        ; --- DUMP ---
        ldy  #KWDUMP
        jsr  MATCHWORD
        bcs  RCTRYMEMORY
        lda  #2                 ; DUMP -> CMDID=2
        sta  CMDID
        rts

RCTRYMEMORY:
        ; --- MEMORY ---
        ldy  #KWMEMORY
        jsr  MATCHWORD
        bcs  RCTRYMEM
        lda  #3                 ; MEMORY -> CMDID=3
        sta  CMDID
        rts

RCTRYMEM:
        ; --- MEM ---
        ldy  #KWMEM
        jsr  MATCHWORD
        bcs  RCTRYLOAD
        lda  #3                 ; MEM -> CMDID=3
        sta  CMDID
        rts

RCTRYLOAD:
        ; --- LOAD ---
        ldy  #KWLOAD
        jsr  MATCHWORD
        bcs  RCTRYHELP
        lda  #4                 ; LOAD -> CMDID=4
        sta  CMDID
        rts

RCTRYHELP:
        ; --- HELP ---
        ldy  #KWHELP
        jsr  MATCHWORD
        bcs  RCTRYRUN
        lda  #5                 ; HELP -> CMDID=5
        sta  CMDID
        rts

RCTRYRUN:
        ; --- RUN ---
        ldy  #KWRUN
        jsr  MATCHWORD
        bcs  RCTRYSAVE
        lda  #1                 ; RUN -> CMDID=1
        sta  CMDID
        rts

RCTRYSAVE:
        ldy  #KWSAVE
        jsr  MATCHWORD
        bcs  RCTRYCOPY
        lda  #6
        sta  CMDID
        rts

RCTRYCOPY:
        ldy  #KWCOPY
        jsr  MATCHWORD
        bcs  RCTRYFILL
        lda  #7                ; COPY -> CMDID=7
        sta  CMDID
        rts       

RCTRYFILL:
        ldy  #KWFILL
        jsr  MATCHWORD
        bcs  RCERR
        lda  #8                ; FILL -> CMDID=8
        sta  CMDID
        rts      

RCERR:
        clr  CMDID              ; tanimsiz komut
        rts



; READADDR: 1..4 hex nibble oku → TMPH:TMPL (16-bit). Hexleri echo eder.
; Basari: C=0, TMPH:TMPL hazir, ARGCNT=okunan nibble sayisi (1..4)
; Hata  : C=1 (hic nibble okunmadi)
READADDR:
        clr  TMPH              ; value = 0
        clr  TMPL
        clr  ARGCNT            ; nibble sayaci

RALOOP:
        jsr  SPEEK
        beq  RAEOF            ; satir sonu → en az 1 nibble varsa basari

        ; A karakteri hex mi? ('0'..'9','A'..'F')
        pshs a                 ; orijinali sakla
        cmpa #'0'
        blo  RADELIM          ; hex degil → ayirac kabul et
        cmpa #'9'
        bls  RAHEXNUM         ; '0'..'9'

        cmpa #'A'
        blo  RADELIM
        cmpa #'F'
        bls  RAHEXALP         ; 'A'..'F'

        ; hex degil → ayirac
RADELIM:
        puls a                 ; orijinali at
        lda  ARGCNT
        beq  RADONENONE      ; hic nibble yok → hata
        clc                    ; en az 1 nibble vardi → basari
        rts

; '0'..'9' → nibble = A - '0'
RAHEXNUM:
        suba #'0'              ; A = 0..9
        bra  RAHAVENIB

; 'A'..'F' → nibble = A - 'A' + 10
RAHEXALP:
        suba #'A'
        adda #10               ; A = 10..15

; A = nibble (0..15) geldi
RAHAVENIB:
        sta  TOK               ; TOK = nibble
        puls a                 ; orijinal hex karakteri geri al
        jsr  OUTCH             ; echo
        jsr  SADV              ; karakteri tuket
        inc  ARGCNT

        ; value <<= 4  (4 kez: LSL TMPL / ROL TMPH)
        ldb  #4
RASHIFT4:
        lsl  TMPL
        rol  TMPH
        decb
        bne  RASHIFT4

        ; value += nibble
        lda  TMPL
        adda TOK
        sta  TMPL
        bcc  RANEXT
        inc  TMPH

RANEXT:
        lda  ARGCNT
        cmpa #4
        blo  RALOOP           ; 4 nibble’dan azsa devam
        clc                    ; 4 nibble tamamlandi → basari
        rts

; satir sonu geldi
RAEOF:
        lda  ARGCNT
        beq  RADONENONE      ; hic nibble yok → hata
        clc                    ; en az 1 nibble var → basari
        rts

RADONENONE:
        sec
        rts
;----------------------READADDR BURADA BİTİYOR---------------------        

; PARSEARGS: Komutun argumanlarini okur ve yerlestirir
; C=1 hata durumunda, C=0 basari

PARSEARGS:
        lda  CMDID              ; komut id
        lbeq  PAERR              ; tanimsiz

        cmpa #4                 ; LOAD: arguman yok
        lbeq  PAOK

        cmpa #5                 ; HELP: arguman yok
        lbeq  PAOK

        cmpa #1                 ; RUN: 1 adres
        bne  PANOTR
        jsr  SKIPSP             ; ayraclari atla
        jsr  READADDR           ; TMPH:TMPL = adres
        lbcs  PAERR              ; hic nibble yoksa hata
        lda  TMPH               ; ARGW0 = adres
        sta  ARGW0HI
        lda  TMPL
        sta  ARGW0LO
        rts

PANOTR: cmpa #2                 ; DUMP: 2 adres (baslangic, bitis)
        bne  PANOTD
        jsr  SKIPSP
        jsr  READADDR           ; ilk adres
        lbcs  PAERR
        lda  TMPH
        sta  ARGW0HI
        lda  TMPL
        sta  ARGW0LO
        jsr  SKIPSP
        jsr  READADDR           ; ikinci adres
        lbcs  PAERR
        lda  TMPH
        sta  ARGW1HI
        lda  TMPL
        sta  ARGW1LO
        rts

PANOTD: cmpa #3                 ; MEM: 1 adres, baytlar exec'te
        bne  PANOTSAVE
        jsr  SKIPSP
        jsr  READADDR
        lbcs  PAERR
        lda  TMPH
        sta  ARGW0HI
        lda  TMPL
        sta  ARGW0LO
        rts

PANOTSAVE:
        cmpa #6                 ; SAVE: 2 adres (baslangic, bitis)
        bne  PANOTCOPY
        jsr  SKIPSP
        jsr  READADDR           ; ilk adres
        lbcs  PAERR
        lda  TMPH
        sta  ARGW0HI
        lda  TMPL
        sta  ARGW0LO
        jsr  SKIPSP
        jsr  READADDR           ; ikinci adres
        bcs  PAERR
        lda  TMPH
        sta  ARGW1HI
        lda  TMPL
        sta  ARGW1LO
        rts
PANOTCOPY:
        cmpa #7                 ; COPY: 3 adres (src, dst, len)
        bne  PANOTFILL

        jsr  SKIPSP
        jsr  READADDR           ; ARGW0 = src
        bcs  PAERR
        lda  TMPH
        sta  ARGW0HI
        lda  TMPL
        sta  ARGW0LO

        jsr  SKIPSP
        jsr  READADDR           ; ARGW1 = dst
        bcs  PAERR
        lda  TMPH
        sta  ARGW1HI
        lda  TMPL
        sta  ARGW1LO

        jsr  SKIPSP
        jsr  READADDR           ; ARGW2 = len
        bcs  PAERR
        lda  TMPH
        sta  ARGW2HI
        lda  TMPL
        sta  ARGW2LO
        rts

PANOTFILL:
        cmpa #8                 ; FILL: 3 arguman (start, end, val)
        bne  PAERR

        jsr  SKIPSP
        jsr  READADDR           ; ARGW0 = start
        bcs  PAERR
        lda  TMPH
        sta  ARGW0HI
        lda  TMPL
        sta  ARGW0LO

        jsr  SKIPSP
        jsr  READADDR           ; ARGW1 = end
        bcs  PAERR
        lda  TMPH
        sta  ARGW1HI
        lda  TMPL
        sta  ARGW1LO

        jsr  SKIPSP
        jsr  READADDR           ; ARGW2LO = val (1..2 nibble yeterli)
        bcs  PAERR
        clr  ARGW2HI
        lda  TMPL
        sta  ARGW2LO
        rts        

PAOK:   rts                     ; argumansiz komutlar icin
PAERR:  sec                     ; hata
        rts

; ===================== Parser helper: read 2 hex as a byte (echoing) =====================
; SREADBYTE: buffer’dan 2 hex nibble oku → A=byte, echo eder
; C=0 ok, C=1 hata/eksik nibble
SREADBYTE:
        jsr  SKIPSP              ; ayraçları atla

        ; HI nibble
        jsr  SPEEK
        beq  SRBERR
        pshs a
        jsr  HEXCON
        bcs  SRBERRPOP
        puls a
        jsr  OUTCH               ; echo
        jsr  HEXCON              ; A=hi nibble
        asla
        asla
        asla
        asla
        anda #$F0
        sta  TMP                 ; TMP = hi<<4
        jsr  SADV                ; tüket

        ; LO nibble
        jsr  SPEEK
        beq  SRBERR
        pshs a
        jsr  HEXCON
        bcs  SRBERRPOP
        puls a
        jsr  OUTCH               ; echo
        jsr  HEXCON              ; A=lo nibble
        anda #$0F
        ora  TMP                 ; A = (hi<<4)|lo
        jsr  SADV                ; tüket
        clc
        rts

SRBERRPOP:
        puls a                   ; stack’i düzelt
SRBERR:
        sec
        rts

; ===================== EXEC Routines (no parsing inside) =====================

; --- Run: jump to ARGW0 ---
CMDEXECRUN:
        ldx  ARGW0HI
        jsr  CRLF
        jmp  ,x

; --- Dump: dump ARGW0..ARGW1 inclusive (proven D logic) ---
CMDEXECDUMP:
        ; move args into working vars
        lda  ARGW0HI
        sta  addrhi
        lda  ARGW0LO
        sta  addrlo
        lda  ARGW1HI
        sta  LOADSTA
        lda  ARGW1LO
        sta  LOADSTAL
        ldx  addrhi
        ; start>end? → return without output
        lda  addrhi
        cmpa LOADSTA
        bhi  DRET
        blo  DSTART
        lda  addrlo
        cmpa LOADSTAL
        bhi  DRET
DSTART: jsr  CRLF

DNEXTLINE:
        lda  addrhi
        jsr  HEXOUT
        lda  addrlo
        jsr  HEXOUT
        lda  #':'
        jsr  OUTCH
        lda  #' '
        jsr  OUTCH
        lda  #16
        sta  HEXLDREG

DLINELOOP:
        lda  addrhi
        cmpa LOADSTA
        bhi  DFINISHLINE
        blo  DPRINT
        lda  addrlo
        cmpa LOADSTAL
        bhi  DFINISHLINE

DPRINT: lda  ,x
        jsr  HEXOUT
        lda  #' '
        jsr  OUTCH
        leax 1,x
        ; addr++
        lda  addrlo
        adda #1
        sta  addrlo
        bne  DCOUNT
        lda  addrhi
        adda #1
        sta  addrhi
DCOUNT: dec  HEXLDREG
        bne  DLINELOOP

DFINISHLINE:
        jsr  CRLF
        ; end?
        lda  addrhi
        cmpa LOADSTA
        bhi  DRET
        blo  DNEXTLINE
        lda  addrlo
        cmpa LOADSTAL
        bhi  DRET
        bra  DNEXTLINE
DRET:   rts

; --- Memory Edit: write bytes starting at ARGW0 (exec writes) ---
CMDEXECMEM:
        ; working addr = ARGW0
        lda  ARGW0HI
        sta  addrhi
        lda  ARGW0LO
        sta  addrlo

        ; Visual: print "aaaa " then bytes as written (space-separated)
        jsr  CRLF
        lda  addrhi
        jsr  HEXOUT
        lda  addrlo
        jsr  HEXOUT
        lda  #' '
        jsr  OUTCH

MLOOPX:
        jsr  SKIPSP
        jsr  SPEEK
        beq  MDONEX
        jsr  SREADBYTE
        bcs  MDONEX
        ; PTR = addr
        sta  TMP
        lda  addrhi
        sta  PTR
        lda  addrlo
        sta  PTRL
        lda  TMP
        sta  [PTR]          ; write A to memory[addr]
        ; addr++
        lda  addrlo
        adda #1
        sta  addrlo
        bne  MECHO
        lda  addrhi
        adda #1
        sta  addrhi
MECHO:  lda  #' '
        jsr  OUTCH
        bra  MLOOPX
MDONEX:
        jsr  CRLF
        rts

; --- Loader: unchanged ---
CMDEXECLOAD:
        jsr  CRLF
        lbra LOADERZ

; --- Help: unchanged ---

;Help shows Useful routine adresses Compile adress independently
CMDEXECHELP:
        ldx  #HLPMSG
        jsr  PRINT

        LDX #GETCHMSG
        JSR PRINT
        LDX #GETCH ; HEXOUT getch label adres (olursa tabi)
        JSR PRINTX

        LDX #SPACEMSG
        JSR PRINT

        LDX #OUTCHMSG
        JSR PRINT
        LDX #OUTCH
        JSR PRINTX

        JSR CRLF        

        LDX #PRINTXMSG
        JSR PRINT
        LDX #PRINTX
        JSR PRINTX

        LDX #SPACEMSG
        JSR PRINT

        LDX #PRINTAMSG
        JSR PRINT
        LDX #PRINTA
        JSR PRINTX                

        JSR CRLF

        LDX #DELAYMSG
        JSR PRINT
        LDX #DELAY1S
        JSR PRINTX

        LDX #SPACEMSG
        JSR PRINT

        LDX #PROMPTMSG
        JSR PRINT
        LDX #MON
        JSR PRINTX 

        JSR CRLF

        LDX #HEXOUTMSG
        JSR PRINT
        LDX #HEXOUT
        JSR PRINTX        

        rts

; CMDEXECSAVE: ARGW0..ARGW1 araligini Intel HEX olarak terminale yazar

CMDEXECSAVE: ; SAVE exec
        jsr  CRLF                     ; cikti oncesi satirbasi

        ; baslangic ve bitis degerlerini yerlestir
        lda  ARGW0HI                  ; baslangic HI
        sta  addrhi
        lda  ARGW0LO                  ; baslangic LO
        sta  addrlo
        lda  ARGW1HI                  ; bitis HI
        sta  LOADSTA
        lda  ARGW1LO                  ; bitis LO
        sta  LOADSTAL

SAVENEXTLINE: 
        ; remaining = (END - ADDR) + 1  → TMPH:TMPL
        lda  LOADSTAL                 ; low
        suba addrlo
        sta  TMPL
        lda  LOADSTA                  ; high
        sbca addrhi
        sta  TMPH
        lda  TMPL
        adda #1
        sta  TMPL
        bcc  SLENOKADD
        inc  TMPH
SLENOKADD:
        lda  TMPH
        ora  TMPL
        lbeq  SAVEEND                  ; kalan yoksa EOF

        ; SAVECNT = min(16, remaining)
        lda  TMPH
        bne  SLENSET16
        lda  TMPL
        cmpa #16
        bls  SLENSETVAL
SLENSET16:
        lda  #16
SLENSETVAL:
        sta  SAVECNT

        ; ':' yaz ve checksum'i sifirla
        lda  #':'
        jsr  OUTCH
        clr  SAVECHKS

        ; len yaz + checksum
        lda  SAVECNT
        pshs a
        jsr  HEXOUT
        puls a
        adda SAVECHKS
        sta  SAVECHKS

        ; adres HI yaz + checksum
        lda  addrhi
        pshs a
        jsr  HEXOUT
        puls a
        adda SAVECHKS
        sta  SAVECHKS

        ; adres LO yaz + checksum
        lda  addrlo
        pshs a
        jsr  HEXOUT
        puls a
        adda SAVECHKS
        sta  SAVECHKS

        ; record type = 00 (toplama etkisi yok, sadece yaz)
        lda  #$00
        jsr  HEXOUT

        ; veri baytlari
        ldx  addrhi                  ; X = ADDR
        ldb  SAVECNT                 ; B = sayac
SAVEDATA:
        lda  ,x                      ; A = veri
        pshs a
        jsr  HEXOUT                  ; veriyi yaz
        puls a
        adda SAVECHKS                ; checksum guncelle
        sta  SAVECHKS
        leax 1,x                     ; X++
        lda  addrlo                  ; ADDR++
        adda #1
        sta  addrlo
        bcc  SDNC
        lda  addrhi
        adda #1
        sta  addrhi
SDNC:
        decb
        bne  SAVEDATA

        ; checksum = two's complement of sum
        lda  SAVECHKS
        nega
        jsr  HEXOUT

        jsr  CRLF                    ; satir sonu

        ; devam kosulu: ADDR <= END
        lda  addrhi
        cmpa LOADSTA
        lblo  SAVENEXTLINE
        bhi  SAVEEND
        lda  addrlo
        cmpa LOADSTAL
        lbls  SAVENEXTLINE

SAVEEND:
        ; EOF rekordu: :00000001FF
        lda  #':'
        jsr  OUTCH
        lda  #$00
        jsr  HEXOUT                  ; len
        lda  #$00
        jsr  HEXOUT                  ; addr HI
        lda  #$00
        jsr  HEXOUT                  ; addr LO
        lda  #$01
        jsr  HEXOUT                  ; type
        lda  #$FF
        jsr  HEXOUT                  ; checksum
        jsr  CRLF
        rts    
;--------------------SAVE ENDS HERE---------------------------


; CMDEXECCOPY: ARGW0=src, ARGW1=dst, ARGW2=len  (len bayti kopyalar)
; Overlap guvenli: eger dst araligina giriyorsa geri kopya yapar
CMDEXECCOPY:    ; COPY exec
        jsr  CRLF

        ; src -> X
        ldx  ARGW0HI
        ; dst -> U
        ldu  ARGW1HI
        ; len -> TMPH:TMPL
        lda  ARGW2HI
        sta  TMPH
        lda  ARGW2LO
        sta  TMPL

        ; len=0 ise cik
        lda  TMPH
        ora  TMPL
        lbeq  CPEXIT

        ; src+len -> addrhi:addrlo  (ust sinir, dahil degil)
        lda  ARGW0LO
        adda TMPL
        sta  addrlo
        lda  ARGW0HI
        adca TMPH
        sta  addrhi

        ; --- yon secimi ---
        ; dst < src  -> ileri kopya
        lda  ARGW1HI
        cmpa ARGW0HI
        blo  CPFWD
        bhi  CPCHKOVER2         ; dst high > src high ise ileride kontrol et
        ; high esit -> low karsilastir
        lda  ARGW1LO
        cmpa ARGW0LO
        blo  CPFWD              ; dst low < src low -> ileri

CPCHKOVER2:
        ; simdi dst >= src
        ; eger dst < src+len ise OVERLAP -> geri kopya, aksi halde ileri
        lda  ARGW1HI
        cmpa addrhi
        blo  CPBWD              ; dst high < (src+len) high -> geri
        bhi  CPFWD              ; dst high > (src+len) high -> ileri
        lda  ARGW1LO
        cmpa addrlo
        blo  CPBWD              ; dst low < (src+len) low -> geri
        ; dst >= src+len -> ileri
        bra  CPFWD

; ---------- ileri kopya (low..high) ----------
CPFWD:
CPFWDLOOP:
        lda  TMPH
        ora  TMPL
        beq  CPEXIT
        lda  ,x
        sta  ,u
        leax 1,x
        leau 1,u
        ; len--
        lda  TMPL
        suba #1
        sta  TMPL
        bcc  CPFWDLOOP
        dec  TMPH
        bra  CPFWDLOOP

; ---------- geri kopya (high..low) ----------
CPBWD:
        ; X = src + len - 1  (addrhi:addrlo su an src+len)
        lda  addrlo
        suba #1
        sta  addrlo
        bcc  CPBWDX
        lda  addrhi
        suba #1
        sta  addrhi
CPBWDX: ldx  addrhi

        ; U = dst + len - 1  (PTR:PTRL hesap icin kullaniliyor)
        lda  ARGW1LO
        adda TMPL
        sta  PTRL
        lda  ARGW1HI
        adca TMPH
        sta  PTR
        lda  PTRL
        suba #1
        sta  PTRL
        bcc  CPBWDU
        lda  PTR
        suba #1
        sta  PTR
CPBWDU: ldu  PTR

CPBWDLOOP:
        lda  TMPH
        ora  TMPL
        beq  CPEXIT
        lda  ,x
        sta  ,u
        leax -1,x
        leau -1,u
        ; len--
        lda  TMPL
        suba #1
        sta  TMPL
        bcc  CPBWDLOOP
        dec  TMPH
        bra  CPBWDLOOP

CPEXIT:
        rts

;---------------------------COPY COMMAND ENDS HERE----------------


;-----------------------------FILL COMMAND------------------------

; CMDEXECFILL: [ARGW0 .. ARGW1] dahil araligini ARGW2LO degeriyle doldurur
CMDEXECFILL:     ; FILL exec
        jsr  CRLF

        ; start -> addrhi:addrlo
        lda  ARGW0HI
        sta  addrhi
        lda  ARGW0LO
        sta  addrlo

        ; end -> LOADSTA:LOADSTAL
        lda  ARGW1HI
        sta  LOADSTA
        lda  ARGW1LO
        sta  LOADSTAL

        ; byte deger -> B
        lda  ARGW2LO
        tfr  a,b

        ; X = addr
        ldx  addrhi

FILLLOOP:
        ; addr > end ? bitir
        lda  addrhi
        cmpa LOADSTA
        blo  FILLSTORE
        bhi  FILLDONE
        lda  addrlo
        cmpa LOADSTAL
        bhi  FILLDONE

FILLSTORE:
        stb  ,x                 ; degeri yaz
        leax 1,x                ; X++
        ; addr++
        lda  addrlo
        adda #1
        sta  addrlo
        bcc  FILLLOOP
        lda  addrhi
        adda #1
        sta  addrhi
        bra  FILLLOOP

FILLDONE:
        rts

;---------------------FILL ENDS HERE-----------------------        

; ===================== MONITOR MAIN — uses Kernel then Exec =====================
MON:
        lda  #$0D
        jsr  OUTCH
        lda  #$0A
        jsr  OUTCH
        ldx  #PMON
        jsr  PRINT

        jsr  READLINE        ; fill INBUF
        jsr  SINIT           ; INPOS=0

        jsr  READCMD         ; detect R/D/M/L/H
        lda  CMDID
        beq  MONERR          ; unknown → '?'

        jsr  PARSEARGS       ; fill ARGW0/ARGW1
        bcs  MONERR

        ; dispatch (exec only — no parsing here)
        lda  CMDID
        cmpa #1
        beq  DORUN
        cmpa #2
        beq  DODUMP
        cmpa #3
        beq  DOMEM
        cmpa #4
        beq  DOLOAD
        cmpa #5
        beq  DOHELP
        cmpa #6         ;SAVE
        beq  DOSAVE
        cmpa #7
        beq  DOCOPY
        cmpa #8
        beq  DOFILL
        bra  MON
                
        

DORUN:  jsr  CMDEXECRUN
        bra  MON
DODUMP: jsr  CMDEXECDUMP
        bra  MON
DOMEM:  jsr  CMDEXECMEM
        lbra MON
DOLOAD: jsr  CMDEXECLOAD
        bra  MON
DOHELP: jsr  CMDEXECHELP
        bra  MON
DOSAVE: jsr  CMDEXECSAVE
        bra  MON 
DOCOPY: jsr  CMDEXECCOPY
        bra  MON
DOFILL: jsr  CMDEXECFILL
        bra  MON                


MONERR:
        ldx  #PHELP
        jsr  PRINT
        bra  MON

;============MON ENDS HERE=======================

;-----USEFUL MACHINE CODE WRITING ROUTINES-------
; DELAY1S7350: ~1 saniye gecikme @ 7.350 MHz, tum register'lari korur
DELAY1S:         
        pshs  a,b,x

        ldx   #1500              ; dis sayaç (DECIMAL)
DLOOP:
        ldb   #0                  ; ic sayaç = 256 adim
DINNER:
        decb                       ; 2 cy
        bne   DINNER               ; 3 cy (alinarak), son tur 2 cy
        leax  -1,x                 ; 3 cy
        bne   DLOOP                ; 3 cy (alinarak), son tur 2 cy

        puls  x,b,a
        rts
;-------------DELAY1S FINISHED HERE-----------
PRINTA: ;Prints Aregister content as HEX
        jsr HEXOUT
        RTS

PRINTX: ;Prints X register content as 4 digit Hex
; X register degerini 4 hane "HHLL" olarak ekrana yazar.
;           Tum register'lari korur, cikista X dahil hicbiri degismez.

        pshs  a,x,b    ; tumunu koru
        tfr   x,d                ; D = X  (A=HI, B=LO)
;D registerinin üst yarısı: A, alt yarısı B den oluşur
        ; yuksek bayti yaz
        jsr   HEXOUT             ; A’daki bayti "XX" basar

        ; dusuk bayti yaz
        tfr   b,a
        jsr   HEXOUT

        puls  b,x,a
        rts        

;-----USEFUL MACHINE CODE  ROUTINES ENDED-------
           org  $fff0      ; vector table

            fdb  $0000      ; Reserved
            fdb  $0000      ; SWI3
            fdb  $0000      ; SWI2
            fdb  $0000      ; FIRQ
            fdb  $0000      ; IRQ
            fdb  $0000      ; SWI
            fdb  $0000      ; NMI
            fdb  reset      ; RESET
