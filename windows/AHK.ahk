;        d8888          888            888    888          888    888    d8P
;       d88888          888            888    888          888    888   d8P
;      d88P888          888            888    888          888    888  d8P
;     d88P 888 888  888 888888 .d88b.  8888888888  .d88b.  888888 888d88K      .d88b.  888  888
;    d88P  888 888  888 888   d88""88b 888    888 d88""88b 888    8888888b    d8P  Y8b 888  888
;   d88P   888 888  888 888   888  888 888    888 888  888 888    888  Y88b   88888888 888  888
;  d8888888888 Y88b 888 Y88b. Y88..88P 888    888 Y88..88P Y88b.  888   Y88b  Y8b.     Y88b 888
; d88P     888  "Y88888  "Y888 "Y88P"  888    888  "Y88P"   "Y888 888    Y88b  "Y8888   "Y88888
;                                                                                           888
;                                                                                      Y8b d88P
;                                                                                       "Y88P"

;; Media keys
#PgDn::
send, {Volume_Down}
return

#PgUp::
send, {Volume_Up}
return

#Home::
send, {Media_Prev}
return

#End::
send, {Media_Next}
return

#Insert::
send, {Media_Play_Pause}
return

#Delete::
send, {Media_Stop}
return

#f::
run, firefox
return

#Enter::
send, duckduckgo.com {Enter}
return

#t::
run, powershell
sleep, 500
send, cd $HOME {Enter}
return

;;; This doesn't work quite right
;; AppsKey::
;; send, {RWin}
;; return

:*:teh::the
