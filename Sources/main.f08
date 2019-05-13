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

  !УЗНАЕМ КАТАЛОГ, ИЗ КОТОРОГО ЗАПУЩЕНА ПРОГРАММА
  !Инициализируем/сбрасываем переменные
  back=.true.
  call getarg(0,inpath) !получаем папку, из которой запущена программа
  write(*,*) trim(inpath)
    scanVal = scan (inpath, '\', back)
   if (scanVal .gt. 0) then
       mydir=inpath(1:scanVal)
   else
       call getcwd(mydir)
   endif

  !ВЫВОДИМ ЛОГОТИП ПРОГРАММЫ ПЕРЕД КАЖДЫМ ЗАПУСКОМ
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


  !ФОРМИРУЕМ ПЕРВОНАЧАЛЬНЫЕ ДАННЫЕ ОБ ИЗОТОПАХ
  !----------------------------------
  call FormJARFRisotopicTable
  !----------------------------------

  !ИНИЦИАЛИЗАЦИЯ
  !----------------------------------
  call InitialMovements
  !----------------------------------

  !УДАЛЯЕМ ДОЛЛАРЫ ИЗ ВХОДНОГО ФАЙЛА JARFR
  !if (isTEST >= 2) write(*, *) 'Running function InputFileDelDollars(inp1)...'
  !----------------------------------
  if (StepGlobal==1) call InputFileDelDollars(inp1,inp_temp)

  !----------------------------------
  !if (isTEST >= 2) write(*, *) 'InputFileDelDollars(inp1) - OK!'
  !УДАЛЯЕМ ДОЛЛАРЫ ИЗ ВХОДНОГО ФАЙЛА REPRORYV
  !if (isTEST >= 2) write(*, *) 'Running function InputFileDelDollars(inpRECL)...'
  !----------------------------------
  if (StepGlobal==1) call InputFileDelDollars(inpRECL,inpRECL_temp)
  !----------------------------------
  !if (isTEST >= 2) write(*, *) 'InputFileDelDollars(inpRECL) - OK!'

  input_file1 = inp_temp



!ВОЗВРАЩАЕМСЯ СЮДА НА КАЖДОМ НОВОМ ШАГЕ
11 call PreStepShow

  !ВНОСИМ ДАННЫЕ ИЗ ВХОДНОГО ФАЙЛА JARFR И REPRORYV В ПЕРЕМЕННЫЕ В ПРОГРАММЕ
  write(333, *) 'READ JARFR INPUT FILE ...'
  !----------------------------------
  call ReadJARFRInputData(input_file1)
  !----------------------------------
  write(333, *) 'READ JARFR INPUT FILE DONE!'

  !ПЕРЕД ТЕМ, КАК ЧТО-ЛИБО ДЕЛАТЬ ПОСЛЕ ПЕРВОГО ШАГА, НУЖНО СНАЧАЛА ВНЕСТИ СТАРЫЕ ЗНАЧЕНИЯ КОНЦЕНТРАЦИЙ В МАССИВ AD
  if (StepGlobal > 1) then
    write(333, *) 'COPING OLD CONCENTRATIONS...'
    !----------------------------------
    call CopyOldConc
    !----------------------------------
    write(333, *) 'COPING OLD CONCENTRATIONS DONE'
  end if


  !ЕСЛИ УЖЕ МОЖНО РАССЧИТАТЬ КОНЦЕНТРАЦИИ ПЕРЕРАБОТАННЫХ ТВС, ТО ТОГДА ЗАПУСКАЕМ ПРОЦЕДУРУ ПЕРЕРАБОТКИ (расчёта переработанных концентраций)
  if (.not.allocated(IsotReclConc)) allocate(IsotReclConc(1:Ner1))
  IsotReclConc = IsotReclConc(1:Ner1)
  if (StepGlobal > (nVRH+nRecl+1)) then
    write(333, *) 'RECYCLING PROCEDURE...'
    !----------------------------------
    call DoRecycle
    !----------------------------------
    write(333, *) 'RECYCLING PROCEDURE DONE!'
  end if


  !ФОРМИРУЕМ НА КАЖДОМ НОВОМ ШАГЕ ВХОДНОЙ ФАЙЛ ДЛЯ JARFR НА ОСНОВЕ ВХОДНЫХ ДАННЫХ REPRORYV
  write(333, *) 'FORM NEW JARFR INPUT FILE...'
  !----------------------------------
  call FormNewInputJARFRFile
  !----------------------------------
  write(333, *) 'FORM NEW JARFR INPUT FILE DONE!'


  !ПРОВЕРЯЕМ ОШИБКИ ВВОДА
  if (StepGlobal == 1) then
    RECL_ERR_volume1 = 0
    RECL_ERR_volume2 = 0
  end if
  isError = GetErrors() !(проверяем на наличие ошибок во входных данных)
  if (isError.ne.0) goto 12
  RECL_ERR_FizZone1 = RECL_ERR_FizZone2 !сохраняем текущее значение перегружаемых зон как значение на предыдущем шаге



  !ТЕКУЩИЙ ШАГ ЗЯТЦ
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
  write (unit=fmtOf_STR, FMT=fmtStr2) AllSteps !переводим число MkCamp*nCamp в строку fmtOf_STR

  !if (StepGlobal == 1) goto 13


  !ВЫВОДИМ НА КРИТИЧНОСТЬ, ЕСЛИ ЗАДАНО
  if (CritOn.EQ.1) then
    if (StepGlobal >= StartStep) then
      write(333, *) 'GO TO CRITICALLY CORE...'
      !----------------------------------
      call Crit
      !----------------------------------
      write(333, *) 'GO TO CRITICALLY CORE DONE!'
    else
      !копируем, если у нас включен расчёт
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




  !ЗАПУСК JARFR
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
    !КОПИРУЕМ ВХОДНЫЕ/ВЫХОДНЫЕ ФАЙЛЫ ИЗ КОРНЯ В ПАПКУ ШАГА
    !----------------------------------
    call CopyResFilesToRecycleFolder
    !----------------------------------
  else
    write(*, *) 'NOT RUNNING JARFR FOR STEP '//trim(StepGlobal_STR)//' OF '//fmtOf_STR//'.'//&
              & '(See StartStep in REPRORYV input file)'
  end if


  !СЧИТАЕМ ЭФФЕКТЫ РЕАКТИВНОСТИ ДЛЯ ДАННОЙ ЗАДАЧИ
  if ((DoplerOn.EQ.1).OR.(DensKoeffOn.EQ.1)) then
    if (StepGlobal >= StartStepReactivity) then
      !ФОРМИРУЕМ ВТОРОЙ СПЕЦИАЛЬНЫЙ ВХОДНОЙ ФАЙЛ ДЛЯ ПАРАЛЛЕЛЬНОГО ЗАПУСКА JARFR ДЛЯ АНАЛИЗА ЭФФЕКТОВ РЕАКТИВНОСТИ (ДОПЛЕР, ПЛОТНОСТНЫЕ)
      write(333, *) 'CREATING JARFR INPUT FILES FOR REACTIVITY EFFECTS ANALYSIS...'
      !----------------------------------
      call FormReactivityInputJARFRFile
      !----------------------------------
      write(333, *) 'CREATING JARFR INPUT FILES FOR REACTIVITY EFFECTS ANALYSIS DONE!'

      !ЗАПУСКАЕМ JARFR ДЛЯ АНАЛИЗА ЭФФЕКТОВ РЕАКТИВНОСТИ
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



  !ДЛЯ СЛЕДУЮЩЕГО ШАГА СОХРАНЯЕМ ВХОДНОЙ ФАЙЛ, КОТОРЫЙ МЫ ТОЛЬКО ЧТО ЗАПУСТИЛИ (на основе него потом будет делаться входной файл для следующего шага)
  input_file1 = trim(CurDir)//'\'//trim(StepGlobal_STR)//'stp_jarfr_input.txt'


  !ОБРАБАТЫВАЕМ ВЫХОДНОЙ ФАЙЛ JARFR
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


  !ПРОВЕРЯЕМ ОШИБКИ В РЕЗУЛЬТИРУЮЩИХ ФАЙЛАХ И УДАЛЯЕМ ВЫХОДНЫЕ ФАЙЛЫ ИЗ КОРНЕВОЙ ПАПКИ
  if (isTEST >= 2) write(333, *) 'RECL_ERR_volume1=',RECL_ERR_volume1
  isError = GetErrors() !(проверяем на наличие ошибок во входных данных)
  if (isError.ne.0) goto 12
  RECL_ERR_volume1 = RECL_ERR_volume2 !сохраняем выгруженный объем для следующего шага


  !УДАЛЯЕМ ВСЕ ВРЕМЕННЫЕ ВХОДНЫЕ/ВЫХОДНЫЕ ФАЙЛЫ ИЗ КОРНЯ
  !----------------------------------
  write(333, *) 'DELETE TEMP FILES'
  call DelTempFilesAll
  write(333, *) 'DELETE TEMP FILES - OK'
  !----------------------------------


  !ЕСЛИ НУЖНО, ТО ЗАПУСКАЕМ BPSD ДЛЯ РАСЧЁТА ВЫГОРАНИЯ
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

  !Удаляем временный файл tmp
  re_i = system(trim(DelComm)//" tmp 1>nul")

  !нужно ли сбрасывать счетчик текущей мирокампаний
  MkCampCounter = MkCampCounter + 1
  if (MkCampCounter > MkCamp) MkCampCounter = 1




  !if (StepGlobal == 1) goto 13




  !re_i = system("pause")
  !НУЖНО ЛИ ЗАКОНЧИТЬ РАСЧЁТ
  StepGlobal = StepGlobal + 1
  if (StepGlobal <= (MkCamp*nCamp)) then
    goto 11
  else
    goto 13
  end if

  !ОБРАБОТКА КОДОВ ОШИБОК
12 call ShowErrorFromCode(isError)

  !СОХРАНЯЕМ ОСНОВЫНЕ ДАННЫЕ В ФАЙЛ alldata.txt
13 call GetTotalResults !сохраняем окончательный результат в один общий файл

  close(333)

  !СОХРАНЯЕМ БАЛАНСОВЫЙ РАСЧЁТ В ФАЙЛ
  call SaveBalance
  write(*,*)
  write(*,*) "-----------------------"
  write(*,*) "REPRORYV END ITS WORK!"
  write(*,*) "VERSION: ",CONST_VERSION
  write(*,*) "RELEASE DATE: ",CONST_DATE
  write(*,*) "(C) IVAN SALDIKOV"

  !re_i = system("pause")
end program main
