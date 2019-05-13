! REPRORYV
! By IVAN SALDIKOV


!>MAIN PROGRAM
program main
  use CommonModJARFR

  integer scanVal
  logical back
  character*180 inpath
  character*500 TITLE_CAPTION(1:10)
  character*180 input_file1
  character*180 InpFileName
  integer :: isError=0, GetErrors !<checking if error of input
  integer :: AllSteps
  character*4 fmtStr2, fmtOf_STR
  character*35 HOUR1, HOUR2     !<initial and end time of calculation

  !”«Õ¿≈Ã  ¿“¿ÀŒ√, »«  Œ“Œ–Œ√Œ «¿œ”Ÿ≈Õ¿ œ–Œ√–¿ÃÃ¿
  !»ÌËˆË‡ÎËÁËÛÂÏ/Ò·‡Ò˚‚‡ÂÏ ÔÂÂÏÂÌÌ˚Â
  back=.true.
  call getarg(0,inpath) !ÔÓÎÛ˜‡ÂÏ Ô‡ÔÍÛ, ËÁ ÍÓÚÓÓÈ Á‡ÔÛ˘ÂÌ‡ ÔÓ„‡ÏÏ‡
  write(*,*) trim(inpath)
    scanVal = scan (inpath, '\', back)
   if (scanVal .gt. 0) then
       mydir=inpath(1:scanVal)
   else
       call getcwd(mydir)
   endif

  !¬€¬Œƒ»Ã ÀŒ√Œ“»œ œ–Œ√–¿ÃÃ€ œ≈–≈ƒ  ¿∆ƒ€Ã «¿œ”— ŒÃ
  TITLE_CAPTION(1) = '                                                                             '
  TITLE_CAPTION(2) = '                                                                             '
  TITLE_CAPTION(3) = '============================================================================='
  TITLE_CAPTION(4) = '      _____  ______ _____  _____   ____  _______     ____      __            '
  TITLE_CAPTION(5) = '     |  __ \|  ____|  __ \|  __ \ / __ \|  __ \ \   / /\ \    / /            '
  TITLE_CAPTION(6) = '     | |__) | |__  | |__) | |__) | |  | | |__) \ \_/ /  \ \  / /             '
  TITLE_CAPTION(7) = '     |  _  /|  __| |  ___/|  _  /| |  | |  _  / \   /    \ \/ /              '
  TITLE_CAPTION(8) = '     | | \ \| |____| |    | | \ \| |__| | | \ \  | |      \  /               '
  TITLE_CAPTION(9) = '     |_|  \_\______|_|    |_|  \_\\____/|_|  \_\ |_|       \/                '
 TITLE_CAPTION(10) = '-----------------------------------------------------------------------------'
  do i=1,10
    write(*,*) trim(TITLE_CAPTION(i))
  end do
  write(*,*) "REPRORYV STARTS!"
  write(*,*) "VERSION: ",CONST_VERSION
  write(*,*) "RELEASE DATE: ",CONST_DATE
  write(*,*) "(C) IVAN SALDIKOV"


  !‘Œ–Ã»–”≈Ã œ≈–¬ŒÕ¿◊¿À‹Õ€≈ ƒ¿ÕÕ€≈ Œ¡ »«Œ“Œœ¿’
  !----------------------------------
  call FormJARFRisotopicTable
  !----------------------------------


  !»Õ»÷»¿À»«¿÷»ﬂ
  !----------------------------------
  call InitialMovements
  !----------------------------------

  !”ƒ¿Àﬂ≈Ã ƒŒÀÀ¿–€ »« ¬’ŒƒÕŒ√Œ ‘¿…À¿ JARFR
  !if (isTEST >= 2) write(*, *) 'Running function InputFileDelDollars(inp1)...'
  !----------------------------------
  if (StepGlobal==1) call InputFileDelDollars(inp1,inp_temp)

  !----------------------------------
  !if (isTEST >= 2) write(*, *) 'InputFileDelDollars(inp1) - OK!'
  !”ƒ¿Àﬂ≈Ã ƒŒÀÀ¿–€ »« ¬’ŒƒÕŒ√Œ ‘¿…À¿ REPRORYV
  !if (isTEST >= 2) write(*, *) 'Running function InputFileDelDollars(inpRECL)...'
  !----------------------------------
  if (StepGlobal==1) call InputFileDelDollars(inpRECL,inpRECL_temp)
  !----------------------------------
  !if (isTEST >= 2) write(*, *) 'InputFileDelDollars(inpRECL) - OK!'

  input_file1 = inp_temp



!¬Œ«¬–¿Ÿ¿≈Ã—ﬂ —ﬁƒ¿ Õ¿  ¿∆ƒŒÃ ÕŒ¬ŒÃ ÿ¿√≈
11 call PreStepShow

  !¬ÕŒ—»Ã ƒ¿ÕÕ€≈ »« ¬’ŒƒÕŒ√Œ ‘¿…À¿ JARFR » REPRORYV ¬ œ≈–≈Ã≈ÕÕ€≈ ¬ œ–Œ√–¿ÃÃ≈
  write(333, *) 'READ JARFR INPUT FILE ...'
  !----------------------------------
  call ReadJARFRInputData(input_file1)
  !----------------------------------
  write(333, *) 'READ JARFR INPUT FILE DONE!'

  !œ≈–≈ƒ “≈Ã,  ¿  ◊“Œ-À»¡Œ ƒ≈À¿“‹ œŒ—À≈ œ≈–¬Œ√Œ ÿ¿√¿, Õ”∆ÕŒ —Õ¿◊¿À¿ ¬Õ≈—“» —“¿–€≈ «Õ¿◊≈Õ»ﬂ  ŒÕ÷≈Õ“–¿÷»… ¬ Ã¿——»¬ AD
  if (StepGlobal > 1) then
    write(333, *) 'COPING OLD CONCENTRATIONS...'
    !----------------------------------
    call CopyOldConc
    !----------------------------------
    write(333, *) 'COPING OLD CONCENTRATIONS DONE'
  end if


  !≈—À» ”∆≈ ÃŒ∆ÕŒ –¿——◊»“¿“‹  ŒÕ÷≈Õ“–¿÷»» œ≈–≈–¿¡Œ“¿ÕÕ€’ “¬—, “Œ “Œ√ƒ¿ «¿œ”— ¿≈Ã œ–Œ÷≈ƒ”–” œ≈–≈–¿¡Œ“ » (‡Ò˜∏Ú‡ ÔÂÂ‡·ÓÚ‡ÌÌ˚ı ÍÓÌˆÂÌÚ‡ˆËÈ)
  if (.not.allocated(IsotReclConc)) allocate(IsotReclConc(1:Ner1))
  IsotReclConc = IsotReclConc(1:Ner1)
  if (StepGlobal > (nVRH+nRecl+1)) then
    write(333, *) 'RECYCLING PROCEDURE...'
    !----------------------------------
    call DoRecycle
    !----------------------------------
    write(333, *) 'RECYCLING PROCEDURE DONE!'
  end if


  !‘Œ–Ã»–”≈Ã Õ¿  ¿∆ƒŒÃ ÕŒ¬ŒÃ ÿ¿√≈ ¬’ŒƒÕŒ… ‘¿…À ƒÀﬂ JARFR Õ¿ Œ—ÕŒ¬≈ ¬’ŒƒÕ€’ ƒ¿ÕÕ€’ REPRORYV
  write(333, *) 'FORM NEW JARFR INPUT FILE...'
  !----------------------------------
  call FormNewInputJARFRFile
  !----------------------------------
  write(333, *) 'FORM NEW JARFR INPUT FILE DONE!'


  !œ–Œ¬≈–ﬂ≈Ã Œÿ»¡ » ¬¬Œƒ¿
  if (StepGlobal == 1) then
    RECL_ERR_volume1 = 0
    RECL_ERR_volume2 = 0
  end if
  isError = GetErrors() !(ÔÓ‚ÂˇÂÏ Ì‡ Ì‡ÎË˜ËÂ Ó¯Ë·ÓÍ ‚Ó ‚ıÓ‰Ì˚ı ‰‡ÌÌ˚ı)
  if (isError.ne.0) goto 12
  RECL_ERR_FizZone1 = RECL_ERR_FizZone2 !ÒÓı‡ÌˇÂÏ ÚÂÍÛ˘ÂÂ ÁÌ‡˜ÂÌËÂ ÔÂÂ„ÛÊ‡ÂÏ˚ı ÁÓÌ Í‡Í ÁÌ‡˜ÂÌËÂ Ì‡ ÔÂ‰˚‰Û˘ÂÏ ¯‡„Â



  !“≈ ”Ÿ»… ÿ¿√ «ﬂ“÷
  AllSteps = MkCamp*nCamp
  SELECT CASE (AllSteps)
   CASE (1:9)
      fmtStr2 = '(I1)'
   CASE (10:99)
      fmtStr2 = '(I2)'
   CASE (100:999)
      fmtStr2 = '(I3)'
   CASE (1000:9999)
      fmtStr2 = '(I4)'
   CASE DEFAULT
      fmtStr2 = '(I4)'
  END SELECT
  write (unit=fmtOf_STR, FMT=fmtStr2) AllSteps !ÔÂÂ‚Ó‰ËÏ ˜ËÒÎÓ MkCamp*nCamp ‚ ÒÚÓÍÛ fmtOf_STR

  !if (StepGlobal == 1) goto 13


  !¬€¬Œƒ»Ã Õ¿  –»“»◊ÕŒ—“‹, ≈—À» «¿ƒ¿ÕŒ
  if (CritOn.EQ.1) then
    if (StepGlobal >= StartStep) then
      write(333, *) 'GO TO CRITICALLY CORE...'
      !----------------------------------
      call Crit
      !----------------------------------
      write(333, *) 'GO TO CRITICALLY CORE DONE!'
    else
      !ÍÓÔËÛÂÏ, ÂÒÎË Û Ì‡Ò ‚ÍÎ˛˜ÂÌ ‡Ò˜∏Ú
      command = 'copy recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)// &
      & 'stp_jarfr_input_Crit.txt '//trim(inp_new_to_run)//' 1>>tmp 2>&1'
      re_i = system(command)
      open(unt1, file = inp_new_to_run, status = 'OLD')
      read(unt1, NML = Mim)
      read(unt1, NML = D26)
      read(unt1, NML = Dan)
      read(unt1, NML = Obr)
      read(unt1, NML = Upbn)
      close(unt1)
    end if
  end if




  !«¿œ”—  JARFR
  if (StepGlobal >= StartStep) then
    write(*, *) 'RUNNING JARFR FOR STEP '//trim(StepGlobal_STR)//' OF '//trim(fmtOf_STR)//'...'
    call time1 (HOUR1)
    write(*, *) '  '//HOUR1
    !if (isTEST >= 2) write(*,*) 'trim(mydir)//\inp_new_to_run = '//trim(mydir)//'\inp_new_to_run'
    InpFileName = trim(mydir)//'\inp_new_to_run'
    !InpFileName = 'inp_new_to_run'
    !----------------------------------
    call RunJARFR(InpFileName)
    !----------------------------------
    write(*, *) 'RUNNING JARFR FOR STEP '//trim(StepGlobal_STR)//' DONE!'
    call time1 (HOUR2)
    write(*, *) '  '//HOUR2
    !re_i = system("pause")
    ! Œœ»–”≈Ã ¬’ŒƒÕ€≈/¬€’ŒƒÕ€≈ ‘¿…À€ »«  Œ–Õﬂ ¬ œ¿œ ” ÿ¿√¿
    !----------------------------------
    call CopyResFilesToRecycleFolder
    !----------------------------------
  else
    write(*, *) 'NOT RUNNING JARFR FOR STEP '//trim(StepGlobal_STR)//' OF '//fmtOf_STR//'.'//&
              & '(See StartStep in REPRORYV input file)'
  end if


  !—◊»“¿≈Ã ›‘‘≈ “€ –≈¿ “»¬ÕŒ—“» ƒÀﬂ ƒ¿ÕÕŒ… «¿ƒ¿◊»
  if ((DoplerOn.EQ.1).OR.(DensKoeffOn.EQ.1)) then
    if (StepGlobal >= StartStepReactivity) then
      !‘Œ–Ã»–”≈Ã ¬“Œ–Œ… —œ≈÷»¿À‹Õ€… ¬’ŒƒÕŒ… ‘¿…À ƒÀﬂ œ¿–¿ÀÀ≈À‹ÕŒ√Œ «¿œ”— ¿ JARFR ƒÀﬂ ¿Õ¿À»«¿ ›‘‘≈ “Œ¬ –≈¿ “»¬ÕŒ—“» (ƒŒœÀ≈–, œÀŒ“ÕŒ—“Õ€≈)
      write(333, *) 'CREATING JARFR INPUT FILES FOR REACTIVITY EFFECTS ANALYSIS...'
      !----------------------------------
      call FormReactivityInputJARFRFile
      !----------------------------------
      write(333, *) 'CREATING JARFR INPUT FILES FOR REACTIVITY EFFECTS ANALYSIS DONE!'

      !«¿œ”— ¿≈Ã JARFR ƒÀﬂ ¿Õ¿À»«¿ ›‘‘≈ “Œ¬ –≈¿ “»¬ÕŒ—“»
      write(*, *) 'RUNNING JARFR [REACTIVITY EFFECTS ANALYSIS] FOR STEP '//trim(StepGlobal_STR)// &
                & ' OF '//trim(fmtOf_STR)//'...'
      call time1 (HOUR1)
      write(*, *) '  '//HOUR1
      InpFileName = trim(mydir)//'\inp_new_to_run_Reactivity'
      !InpFileName = 'inp_new_to_run'
      !----------------------------------
      call RunJARFR(InpFileName)
      !----------------------------------

      re_i = system(trim(DelComm)//" inp_new_to_run_Reactivity_consyst.rez 1>>tmp 2>&1")
      re_i = system(trim(DelComm)//" inp_new_to_run_Reactivity_RESULTS.rez 1>>tmp 2>&1")
      re_i = system(trim(DelComm)//" inp_new_to_run_Reactivity_WEIGHTS.rez 1>>tmp 2>&1")
      re_i = system(trim(DelComm)//" inp_new_to_run_RESULTS_DECODED.rez 1>>tmp 2>&1")
      command = 'copy inp_new_to_run_Reactivity_keff.rez '// &
       & 'recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_keff_Reactivity.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      re_i = system(trim(DelComm)//" inp_new_to_run_Reactivity.dat 1>>tmp 2>&1")
      re_i = system(trim(DelComm)//" inp_new_to_run_Reactivity_keff.rez 1>>tmp 2>&1")
      write(*, *) 'RUNNING JARFR [REACTIVITY EFFECTS ANALYSIS] FOR STEP '//trim(StepGlobal_STR)//' DONE!'
      call time1 (HOUR2)
      write(*, *) '  '//HOUR2
    else
      write(*, *) 'NOT RUNNING JARFR [REACTIVITY EFFECTS ANALYSIS] FOR STEP '//trim(StepGlobal_STR)// &
              &  ' OF '//fmtOf_STR//'. (See StartStep in REPRORYV input file)'
    end if
  end if



  !ƒÀﬂ —À≈ƒ”ﬁŸ≈√Œ ÿ¿√¿ —Œ’–¿Õﬂ≈Ã ¬’ŒƒÕŒ… ‘¿…À,  Œ“Œ–€… Ã€ “ŒÀ‹ Œ ◊“Œ «¿œ”—“»À» (Ì‡ ÓÒÌÓ‚Â ÌÂ„Ó ÔÓÚÓÏ ·Û‰ÂÚ ‰ÂÎ‡Ú¸Òˇ ‚ıÓ‰ÌÓÈ Ù‡ÈÎ ‰Îˇ ÒÎÂ‰Û˛˘Â„Ó ¯‡„‡)
  input_file1 = trim(CurDir)//'\'//trim(StepGlobal_STR)//'stp_jarfr_input.txt'


  !Œ¡–¿¡¿“€¬¿≈Ã ¬€’ŒƒÕŒ… ‘¿…À JARFR
  write(*, *) 'READ JARFR OUTPUT FILE ...'
  write(333, *) 'READ JARFR OUTPUT FILE ...'
  InpFileName = 'recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_results.txt'
  write(333, *) '...',InpFileName
  !InpFileName = 'inp_new_to_run_RESULTS.rez'
  !----------------------------------
  call ReadJARFRResult(InpFileName)
  !----------------------------------
  write(*, *) 'READ JARFR OUTPUT FILE DONE!'
  write(333, *) 'READ JARFR OUTPUT FILE DONE!'


  !œ–Œ¬≈–ﬂ≈Ã Œÿ»¡ » ¬ –≈«”À‹“»–”ﬁŸ»’ ‘¿…À¿’ » ”ƒ¿Àﬂ≈Ã ¬€’ŒƒÕ€≈ ‘¿…À€ »«  Œ–Õ≈¬Œ… œ¿œ »
  if (isTEST >= 2) write(333, *) 'RECL_ERR_volume1=',RECL_ERR_volume1
  isError = GetErrors() !(ÔÓ‚ÂˇÂÏ Ì‡ Ì‡ÎË˜ËÂ Ó¯Ë·ÓÍ ‚Ó ‚ıÓ‰Ì˚ı ‰‡ÌÌ˚ı)
  if (isError.ne.0) goto 12
  RECL_ERR_volume1 = RECL_ERR_volume2 !ÒÓı‡ÌˇÂÏ ‚˚„ÛÊÂÌÌ˚È Ó·˙ÂÏ ‰Îˇ ÒÎÂ‰Û˛˘Â„Ó ¯‡„‡


  !”ƒ¿Àﬂ≈Ã ¬—≈ ¬–≈Ã≈ÕÕ€≈ ¬’ŒƒÕ€≈/¬€’ŒƒÕ€≈ ‘¿…À€ »«  Œ–Õﬂ
  !----------------------------------
  write(333, *) 'DELETE TEMP FILES'
  call DelTempFilesAll
  write(333, *) 'DELETE TEMP FILES - OK'
  !----------------------------------


  !≈—À» Õ”∆ÕŒ, “Œ «¿œ”— ¿≈Ã BPSD ƒÀﬂ –¿—◊®“¿ ¬€√Œ–¿Õ»ﬂ
  !----------------------------------
  if (BpsdOn == 1) then
    write(333, *) 'BPSD PROCEDURE...'
    write(*, *) ''
    write(*, *) ''
    write(*, *) ''
    write(*, *) 'BPSD PROCEDURE...'
    if (StepGlobal >= StartStep) then
      call BPSDProcedure
    else
      write(*, *) 'BPSD was not running, skip this step (see input file of REPRORYV)...'
    end if
    write(*, *) 'BPSD PROCEDURE... DONE'
    write(*, *) ''
    write(*, *) ''
    write(333, *) 'BPSD PROCEDURE... DONE'
  end if

  !”‰‡ÎˇÂÏ ‚ÂÏÂÌÌ˚È Ù‡ÈÎ tmp
  re_i = system(trim(DelComm)//" tmp 1>nul")

  !ÌÛÊÌÓ ÎË Ò·‡Ò˚‚‡Ú¸ Ò˜ÂÚ˜ËÍ ÚÂÍÛ˘ÂÈ ÏËÓÍ‡ÏÔ‡ÌËÈ
  MkCampCounter = MkCampCounter + 1
  if (MkCampCounter > MkCamp) MkCampCounter = 1




  !if (StepGlobal == 1) goto 13




  !re_i = system("pause")
  !Õ”∆ÕŒ À» «¿ ŒÕ◊»“‹ –¿—◊®“
  StepGlobal = StepGlobal + 1
  if (StepGlobal <= (MkCamp*nCamp)) then
    goto 11
  else
    goto 13
  end if

  !Œ¡–¿¡Œ“ ¿  ŒƒŒ¬ Œÿ»¡Œ 
12 call ShowErrorFromCode(isError)

  !—Œ’–¿Õﬂ≈Ã Œ—ÕŒ¬€Õ≈ ƒ¿ÕÕ€≈ ¬ ‘¿…À alldata.txt
13 call GetTotalResults !ÒÓı‡ÌˇÂÏ ÓÍÓÌ˜‡ÚÂÎ¸Ì˚È ÂÁÛÎ¸Ú‡Ú ‚ Ó‰ËÌ Ó·˘ËÈ Ù‡ÈÎ

  close(333)

  !—Œ’–¿Õﬂ≈Ã ¡¿À¿Õ—Œ¬€… –¿—◊®“ ¬ ‘¿…À
  call SaveBalance
  write(*,*)
  write(*,*) "-----------------------"
  write(*,*) "REPRORYV END ITS WORK!"
  write(*,*) "VERSION: ",CONST_VERSION
  write(*,*) "RELEASE DATE: ",CONST_DATE
  write(*,*) "(C) IVAN SALDIKOV"

  !re_i = system("pause")
end program main
