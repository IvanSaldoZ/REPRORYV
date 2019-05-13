!ФОРМИРУЕМ ЗНАЧЕНИЯ КОНЦЕНТРАЦИЙ ДЛЯ НОВОГО ШАГА (КОПИРУЕМ ПРЕДЫДУЩИЕ, ЛИБО ПОСЛЕ ПЕРЕРАБОТКИ СЧИТАЕМ НОВЫЕ)
subroutine CopyOldConc
  use CommonModJARFR
  !Имена файлов с концентрациями
  character*250 :: curFile_CONC, curFile_CONC_TEMP
  !Формат для перевода числа в строку
  character*4 :: fmtStr
  !Двухмерный массив AD - концентраций каждого из номеров составов
  real, allocatable :: ADNEWx2(:,:)
  !Номер читаемой зоны
  integer :: NumZonIn
  !Тип зоны (1 - просто перемещаем, 2 - что-то делаем, 3 - Конструкцонные материалы)
  integer :: ZoneTypeIn
  !Внутренний идентификатор изотопа
  integer :: IsotID
  !Считанная концентрация
  real ConcIn
  !строка для чтения файла
  character*160 StrFile1
  !Строка для обработки файлов
  character*6 AddedStr, AddEndOfOutputFile
  !Значение предыдущего шага
  integer StepGlobalMinusOne
  character*4 :: StepGlobalMinusOne_STR
  !Строки для чтения из файла
  character*11 :: Str11
  character*6 :: Str6
  character*150 :: Str150
  !Нужно скопировать данные с предыдущего шага
  StepGlobalMinusOne = StepGlobal - 1
  SELECT CASE (StepGlobalMinusOne)
   CASE (1:9)
      fmtStr = '(I1)'
   CASE (10:99)
      fmtStr = '(I2)'
   CASE (100:999)
      fmtStr = '(I3)'
   CASE (1000:9999)
      fmtStr = '(I4)'
   CASE DEFAULT
      fmtStr = '(I4)'
  END SELECT
  write (unit=StepGlobalMinusOne_STR, FMT=fmtStr) StepGlobalMinusOne;
  !===============================
  !ОБРАБОТКА МАССИВА СОСТАВОВ AD
  !  НУЖНО: 1. Просто переместить все составы, которые участвовать в переработке на следующем шаге НЕ будут
  !         2.а. Заменить выгруженные на исходные (если Step < Nпереработки), на самом деле оставить их такими, какими они были на предыдущем шаге
  !         2.б. Заменить выгруженные на переработанные (если Step >= Nпереработки)
  !делаем двумерный массив из AD, чтобы легче с ним было обращаться
  k = 1
  allocate(ADNEWx2(1:M1, 1:Ner1))
  do i=1,M1
    do j=1,Ner1
      ADNEWx2(i,j) = Ad(k)
      k = k + 1
    end do
  end do

  !СОХРАНЯЕМ В AD НОВЫЕ КОНЦЕНТРАЦИИ ДЛЯ СЛЕДУЮЩИЕГО ШАГА ИЗ ПРЕДЫДУЩЕГО ШАГА. СНАЧАЛА - ДЕЛЯЩИЕСЯ ИЗОТОПЫ
  curFile_CONC_TEMP = 'recycle\'//trim(StepGlobalMinusOne_STR)//'stp'//'\'//trim(StepGlobalMinusOne_STR)//'_conc' !концентрации
  AddedStr = AddEndOfOutputFile(3) !СНАЧАЛА нас интересует время на момент выгрузки из реактора + небольшой выдержки (3-й тип)
  curFile_CONC = trim(curFile_CONC_TEMP)//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'curFile_CONC to copy for Fissile nuclides=',trim(curFile_CONC)
  open (5,file=trim(curFile_CONC),status='unknown')
10  read (5,'(a150)', end=15) StrFile1
  !если мы дошли до зоны
  if (StrFile1(1:11).eq.'       ЗОНА') then
    read (StrFile1,'(a11,I5)') Str11, NumZonIn !      ЗОНА    1.
    if (isTEST >= 2) write(333, *) 'NumZonIn= ',NumZonIn
    read (5,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    read (5,*)   !'Loaded on Step ', StepGlobal
    !копируем только те концентрации, которые перерабатывать НЕ нужно (т.е. начальные конц. и км - зоны типа 1 и 3)
    if (isTEST >= 2) write(333, *) 'ZoneTypeIn= ',ZoneTypeIn

    do i=1,Ner1 !для каждого делящегося изотопа читаем его данные
      read (5,'(i3.3, e14.5)') IsotID, ConcIn
      ADNEWx2(NumZonIn,i) = ConcIn
      !if (isTEST >= 2) write(333, *) 'Not doing anything'
    end do
    !if (isTEST >= 2) write(333, *) 'ADNEWx2(',NumZonIn,',',i,')= ',ADNEWx2(NumZonIn,i);
    read(5,*)
    read(5,*)
    read(5,*)
    goto 10
  else
    goto 10
  end if
15  close(5)




  if (BpsdOn == 1) then
  !СОХРАНЯЕМ В AD НОВЫЕ КОНЦЕНТРАЦИИ ДЛЯ СЛЕДУЮЩИЕГО ШАГА ИЗ ПРЕДЫДУЩЕГО ШАГА. ТЕПЕРЬ - PU И U ИЗ BPSD
  !берем данные из концентраций, рассчитанных на прошлом шаге с помощью BPSD
  curFile_CONC_TEMP = 'recycle_bpsd\'//trim(StepGlobalMinusOne_STR)//'stp'//'\'//trim(StepGlobalMinusOne_STR)//'_conc' !концентрации
  AddedStr = AddEndOfOutputFile(3) !СНАЧАЛА нас интересует время на момент выгрузки из реактора + небольшой выдержки (3-й тип)
  curFile_CONC = trim(curFile_CONC_TEMP)//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'curFile_CONC to copy for KM=',trim(curFile_CONC)
  open (5,file=trim(curFile_CONC),status='unknown')
11  read (5,'(a150)', end=16) StrFile1
  !если мы дошли до зоны
  if (StrFile1(1:11).eq.'       ЗОНА') then
    read (StrFile1,'(a11,I5)') Str11, NumZonIn !      ЗОНА    1.
    if (isTEST >= 2) write(333, *) 'NumZonIn= ',NumZonIn
    read (5,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    read (5,*)   !'Loaded on Step ', StepGlobal
    !копируем только те концентрации, которые перерабатывать НЕ нужно (т.е. начальные конц. и км - зоны типа 1 и 3)
    if (isTEST >= 2) write(333, *) 'ZoneTypeIn= ',ZoneTypeIn

    do i=1,Ner1 !для каждого изотопа читаем его данные
      read (5,'(i3.3, e14.5)') IsotID, ConcIn
      !пропускаем делящиеся - они у нас уже есть
      if (BpsdModeIsots(i).EQ.1) then
        ADNEWx2(NumZonIn,i) = ConcIn
      end if
      !if (isTEST >= 2) write(333, *) 'Not doing anything'
    end do
    !if (isTEST >= 2) write(333, *) 'ADNEWx2(',NumZonIn,',',i,')= ',ADNEWx2(NumZonIn,i);
    read(5,*)
    read(5,*)
    read(5,*)
    goto 11
  else
    goto 11
  end if
16  close(5)
  end if !if (BpsdOn == 1) then



  !после обработки сохраняем в исходный одномерный массив
  n = 1
  Ad=Ad(1:(M1*Ner1))
  do i=1,M1
    do j=1,Ner1
      Ad(n) = ADNEWx2(i,j)
      n = n + 1
    end do
  end do

  !унчичтожаем временный двумерный массив перед выходом
  deallocate(ADNEWx2)
end subroutine CopyOldConc
