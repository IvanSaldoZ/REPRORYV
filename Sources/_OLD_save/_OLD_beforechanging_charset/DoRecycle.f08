!ПЕРЕРАБОТКА ТОПЛИВА ПОСЛЕ ОЧЕРЕДНОГО ШАГА ПО ВЫГОРАНИЮ (RECYCLE)
subroutine DoRecycle
  use CommonModJARFR
  !Имена файлов с массами
  character*250 :: curFileMassLoaded, curFileMassLoaded_TEMP, curFileMassUnloaded, curFileMassUnloaded_TEMP
  character*250 :: curFileMassLoaded_TEMP_BPSD, curFileMassUnloaded_BPSD, curFileMassUnloaded_TEMP_BPSD, curFileMassLoaded_BPSD
  character*250 :: curFileRecycledData
  !Формат для перевода числа в строку
  character*4 :: fmtStr
  !Номер читаемой зоны
  integer :: NumZonIn, NumZonIn2
  !Тип зоны (1 - просто перемещаем, 2 - что-то делаем, 3 - Конструкцонные материалы)
  integer :: ZoneTypeIn
  !строка для чтения файла
  character*160 :: StrFile1, StrFile2
  !Строка для обработки файлов
  character*6 AddedStr, AddEndOfOutputFile
  !Значение шага, на котором была выгружена ныне переработанная ТВС, которая сейчас загружается в реактора
  integer StepUnload
  !Строка шага, на котором была выгружена ныне переработанная ТВС, которая сейчас загружается в реактора
  character*4 :: StepUnload_STR
  !Значение шага, на котором была загружена переработанная ТВС, которая сейчас загружается в реактора
  integer StepLoad
  !Строка шага, на котором была загружена переработанная ТВС, которая сейчас загружается в реактора
  character*4 :: StepLoad_STR
  !Строки для чтения из файла
  character*6 :: Str6
  character*11 :: Str11
  character*15 :: str15
  character*16 :: str16
  character*150 :: Str150
  !Переменные для чтения файла
  integer :: isot_id
  real :: isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
  real :: zone_volume
  !Суммарный объем извлеченных ТВСок при переработке
  real :: VolumeSumOut
  !Идентификаторы изотопов
  integer, allocatable :: isot_ids(:)
  !Массив суммарной массы каждого изотопа на начало (In) и конец работы в реакторе + выдержки (Out)
  real, allocatable :: IsotMassSumIn(:), IsotMassSumOut(:)
  !Массив суммарной массы группы изотопов на начало (In) и конец работы в реакторе + выдержки (Out)
  real, allocatable :: GroupMassSumIn(:), GroupMassSumOut(:)
  !Массив массы группы изотопов после переработки
  real, allocatable :: GroupMassRecl(:)
  !Массив массы и концентраций каждого из изотопов после переработки
  real, allocatable :: IsotReclMass(:)
  !Массив коэффициентов отношения исходной массы группы к новой (нужно, чтобы найти массу после переработки группы изотопов)
  real, allocatable :: KRecl(:)
  !Атомная масса
  real atomic_mass
  !Коэффициент перевода объема (в JARFR он в м3, в BPSD он в см3)
  real Kvol
  !Отображаемое название изотопа
  character*4 IsotCapt, GetIsotopeCaption
  !отображаем группу только один раз
  integer is_first
  !Delta - разница между изначальной массой и начальной
  real delta_mass
  !Массы
  real MassOfZeroGroupType !масса тех изотопов, которые не сбрасываются
  real MassOf2ndGroupType  !масса тех изотопов, которые мы хотим сбросить на первоначальный уровень
  real MassOf2ndGroupType_OLD  !старая масса тех изотопов, которые мы хотим сбросить на первоначальный уровень
  real MassSumOut          !общая выгруженная масса всех материалов


  !Коэффициент перевода объема (в JARFR он в м3, в BPSD он в см3)
  if (BpsdOn.EQ.1) then
    Kvol = 1
  else
    Kvol = 1E6
  end if


  !Шаг, на котором была выгружена ТВСка, которую мы сейчас планируем загрузить после перарботки
  StepUnload = StepGlobal - (nVRH+nRecl+1) !-1 нужно вычесть, потому что мы уже прибавили к StepGlobal один шаг
  SELECT CASE (StepUnload)
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
  write (unit=StepUnload_STR, FMT=fmtStr) StepUnload
  !чтобы сохранить данные о переработанном топливе, нужно добавить специальный файл
  curFileRecycledData = 'recycle\'//trim(StepGlobal_STR)//'stp'//'\'//trim(StepGlobal_STR)//'_RecycledData.txt'
  if (isTEST >= 2) write(333, *) 'curFileRecycledData to recycle=',trim(curFileRecycledData)
  !шаг выгрузки
  curFileMassUnLoaded_TEMP = 'recycle\'//trim(StepUnload_STR)//'stp'//'\'//trim(StepUnload_STR)//'_mass'
  curFileMassUnloaded_TEMP_BPSD = 'recycle_bpsd\'//trim(StepUnload_STR)//'stp'//'\'//trim(StepUnload_STR)//'_mass'
  AddedStr = AddEndOfOutputFile(4) !данные на текущий момент
  curFileMassUnLoaded = trim(curFileMassUnLoaded_TEMP)//trim(AddedStr)//'.txt'
  curFileMassUnloaded_BPSD = trim(curFileMassUnloaded_TEMP_BPSD)//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'curFileMassUnLoaded to recycle=',trim(curFileMassUnLoaded)
  if (isTEST >= 2) write(333, *) 'curFileMassUnLoaded_BPSD to recycle=',trim(curFileMassUnloaded_BPSD)


  !ПЕРАРАБОТКА. ЭТАП 1. Находим суммарную начальную и конечную массу каждого изотопа во всех зонах
  open (777,file=trim(curFileRecycledData),status='unknown')  !данные о переработанном топливе
  VolumeSumOut = 0
  MassSumOut = 0 !Общая выгруженная масса всех материалов

  !Выделяем память для списка изотопов и их масс для дальнейшего сохранения, а также для групп изотопов
  if (.not.allocated(isot_ids)) allocate(isot_ids(1:Ner1))
  isot_ids = isot_ids(1:Ner1)
  if (.not.allocated(IsotMassSumIn)) allocate(IsotMassSumIn(1:Ner1))
  if (.not.allocated(IsotMassSumOut)) allocate(IsotMassSumOut(1:Ner1))
  IsotMassSumIn = IsotMassSumIn(1:Ner1)
  IsotMassSumOut = IsotMassSumOut(1:Ner1)
  IsotMassSumIn = 0
  IsotMassSumOut = 0
  open (901,file=trim(curFileMassUnLoaded),status='unknown') !данные о топливе на момент переработки
10 read (901,'(a160)',end=15) StrFile2
  !если мы дошли до зоны
  if (StrFile2(1:11).eq.'       ЗОНА') then
    read (StrFile2,'(a11,I5)') Str11, NumZonIn !      ЗОНА    1.
    if (isTEST >= 2) write(333, *) 'MassSum = > NumZonIn= ',NumZonIn
    read (901,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    if (isTEST >= 2) write(333, *) '  MassSum = > ZoneTypeIn= ',ZoneTypeIn
    read (901,'(a16,I4)') str16,StepLoad ! Loaded on Step     1
    !read (StrFile2,'(a15,I4)') str15, StepLoad
    if (isTEST >= 2) write(333, *) '  StepLoad= ',StepLoad
    read (901,*)  !     ----------
    read (901,*)  !                       NC            NF            ВЕС          АЛЬФА         СИГМА-C       СИГМА-F
    !обрабатываем только те массы, которые были выгружены и переработаны
    if (ZoneTypeIn.EQ.2) then
      !Шаг, на котором была в первый раз загружена ТВСка в АЗ, которую мы сейчас планируем загрузить после её перарботки
      if (StepLoad <= 0) StepLoad = 1
      SELECT CASE (StepLoad)
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
      write (unit=StepLoad_STR, FMT=fmtStr) StepLoad
      !if (isTEST >= 2) write(333, *) 'StepLoad_STR = '//trim(StepLoad_STR);
      !для сравнения изначальго содержания, находим шаг, на котором данная ТВС была только загружена (1-й тип)
      curFileMassLoaded_TEMP = 'recycle\'//trim(StepLoad_STR)//'stp'//'\'//trim(StepLoad_STR)//'_mass'
      curFileMassLoaded_TEMP_BPSD = 'recycle_bpsd\'//trim(StepLoad_STR)//'stp'//'\'//trim(StepLoad_STR)//'_mass'
      AddedStr = AddEndOfOutputFile(1)
      curFileMassLoaded = trim(curFileMassLoaded_TEMP)//trim(AddedStr)//'.txt'
      if (isTEST >= 2) write(333, *) 'curFileMassLOADED to recycle=',trim(curFileMassLoaded)
      open (900,file=trim(curFileMassLoaded),status='unknown')  !данные о загруженном топливе
      !ищем строку, отвечающую этому номеру зоны
      ios = 0
      !iostat:
        ! = -1 error: end of file
        ! = -2 error: end of record
      findStrWithZone: do while (ios /= -1)
        read(900, '(a160)', advance = "yes", iostat = ios) StrFile1
        if (ios /= -1) then
          if (StrFile1(1:11).eq.'       ЗОНА') then
            read (StrFile1,'(a11,I5)') Str11, NumZonIn2 !      ЗОНА    1.
            if (NumZonIn2 == NumZonIn) then !если мы нашли нашу зону
              if (isTEST >= 2) write(333, *) '  NumZonIn2= ',NumZonIn2
              !спускаемся немного ниже в соответствии с позицией в другом файле
              read (900,*)! TYPE:            1
              read (900,*)!  Loaded on Step    1
              read (900,*)  !     ----------
              read (900,*)  !                       NC            NF            ВЕС          АЛЬФА         СИГМА-C       СИГМА-F
              exit findStrWithZone
            end if
          end if
        end if
      end do findStrWithZone
      do i=1,Ner1
        !read (900,'(a160)') StrFile1
        !if (isTEST >= 2) write (*,*) StrFile1
        !следующие строки встречаются в массовом файле от JARFR
        read (900,'(a15, I3, 6e14.5)') str15, isot_id, isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
        !if (isTEST >= 2) write (*,*) 'isot_id=',isot_id
        !read (900,'(a15, I3)') str15, isot_id
        isot_ids(i) = isot_id
        !Если включена BPSD, то не считываем массу изотопов Pu и U - считываем их дальше
        if (BpsdOn.EQ.1) then
          if (BpsdModeIsots(i).NE.1) then !тут добавляем обычную массу
            IsotMassSumIn(i) = IsotMassSumIn(i) + isot_mass
          end if
        else
          IsotMassSumIn(i) = IsotMassSumIn(i) + isot_mass
        end if
        !следующие строки встречаются в массовом файле от JARFR
        read (901,'(a15, I3, 6e14.5)') str15, isot_id, isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
        if (BpsdOn.EQ.1) then
          if (BpsdModeIsots(i).NE.1) then !тут добавляем обычную массу
            IsotMassSumOut(i) = IsotMassSumOut(i) + isot_mass
            MassSumOut = MassSumOut + isot_mass
          end if
        else
          IsotMassSumOut(i) = IsotMassSumOut(i) + isot_mass
          MassSumOut = MassSumOut + isot_mass
          if ((isot_id == 235) .OR. (isot_id == 238) .OR. (isot_id == 236) .OR. (isot_id == 234)) then
            MassDataU(StepGlobal)%mass_recycle_before = MassDataU(StepGlobal)%mass_recycle_before + isot_mass
          end if
          if ((isot_id == 239) .OR. (isot_id == 240) .OR. (isot_id == 241) .OR. (isot_id == 242) .OR. (isot_id == 248) &
          & .OR. (isot_id == 246)) then
            MassDataPu(StepGlobal)%mass_recycle_before = MassDataPu(StepGlobal)%mass_recycle_before + isot_mass
          end if
        end if
      end do
      read (900,*)  ! пропускаем строку   СУММА В ЗОНЕ
      read (900,*)  ! пропускаем строку   ------------------------------------------------------
      read (900,*)  ! пропускаем строку   BEC U-235
      read (900,*)  ! пропускаем строку   ВЕС U-238
      read (900,*)  ! пропускаем строку   ВЕС PU
      read (900,*)  ! пропускаем строку   ВЕС PU(9+1)
      !читаем строку "ОБЪЁМ ЗОНЫ"
      read (900,'(a16,1e16.5)') Str16, zone_volume
      VolumeSumOut = VolumeSumOut + zone_volume  !Находим суммарный объем, занимаемый этими изотопами
      RECL_nzon_type(NumZonIn)=5    !теперь можем использовать освободившейся номер зоны
      write (333,*) 'Zone ',NumZonIn,' is empty after recycling. We can use it!'
      !RECL_Fiz_Enabled(NumZonIn)=0
      !rewind 900 !переходим в начало файла, из которого берём исходные данные об этих зонах
      close(900)
    end if
    !re_i = system("pause")
    read (901,*)  ! пропускаем строку   СУММА В ЗОНЕ
    read (901,*)  ! пропускаем строку   ------------------------------------------------------
    read (901,*)  ! пропускаем строку   BEC U-235
    read (901,*)  ! пропускаем строку   ВЕС U-238
    read (901,*)  ! пропускаем строку   ВЕС PU
    read (901,*)  ! пропускаем строку   ВЕС PU(9+1)
    read (901,'(a16,1e16.5)') Str16, zone_volume
    read (901,*)
    read (901,*)
    read (901,*)
    goto 10
  else
    goto 10
  end if
15  close(901)


  !Из BPSD копируем данные только для изотопов Pu и U и их ошибки
  if (BpsdOn.EQ.1) then
  open (901,file=trim(curFileMassUnloaded_BPSD),status='unknown') !данные о топливе на момент переработки
11 read (901,'(a160)',end=16) StrFile2
  !если мы дошли до зоны
  if (StrFile2(1:11).eq.'       ЗОНА') then
    read (StrFile2,'(a11,I5)') Str11, NumZonIn !      ЗОНА    1.
    if (isTEST >= 2) write(333, *) 'MassSum = > NumZonIn= ',NumZonIn
    read (901,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    if (isTEST >= 2) write(333, *) '  MassSum = > ZoneTypeIn= ',ZoneTypeIn
    read (901,'(a16,I4)') str16,StepLoad ! Loaded on Step     1
    !read (StrFile2,'(a15,I4)') str15, StepLoad
    if (isTEST >= 2) write(333, *) '  StepLoad= ',StepLoad
    !обрабатываем только те массы, которые были выгружены и переработаны
    if (ZoneTypeIn.EQ.2) then
      !Шаг, на котором была в первый раз загружена ТВСка в АЗ, которую мы сейчас планируем загрузить после её перарботки
      if (StepLoad <= 0) StepLoad = 1
      SELECT CASE (StepLoad)
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
      write (unit=StepLoad_STR, FMT=fmtStr) StepLoad
      !if (isTEST >= 2) write(333, *) 'StepLoad_STR = '//trim(StepLoad_STR);
      !для сравнения изначальго содержания, находим шаг, на котором данная ТВС была только загружена (1-й тип)
      curFileMassLoaded_TEMP_BPSD = 'recycle_bpsd\'//trim(StepLoad_STR)//'stp'//'\'//trim(StepLoad_STR)//'_mass'
      AddedStr = AddEndOfOutputFile(1)
      curFileMassLoaded_BPSD = trim(curFileMassLoaded_TEMP_BPSD)//trim(AddedStr)//'.txt'
      !if (isTEST >= 2) write(333, *) 'curFileMassLoaded_BPSD to recycle=',trim(curFileMassLoaded_BPSD)
      open (900,file=trim(curFileMassLoaded_BPSD),status='unknown')  !данные о загруженном топливе
      !ищем строку, отвечающую этому номеру зоны
      ios = 0
      !iostat:
        ! = -1 error: end of file
        ! = -2 error: end of record
      findStrWithZone2: do while (ios /= -1)
        read(900, '(a160)', advance = "yes", iostat = ios) StrFile1
        if (ios /= -1) then
          if (StrFile1(1:11).eq.'       ЗОНА') then
            read (StrFile1,'(a11,I5)') Str11, NumZonIn2 !      ЗОНА    1.
            if (NumZonIn2 == NumZonIn) then !если мы нашли нашу зону
              if (isTEST >= 2) write(333, *) '  NumZonIn2= ',NumZonIn2
              !спускаемся немного ниже в соответствии с позицией в другом файле
              read (900,*)! TYPE:            1
              read (900,*)!  Loaded on Step    1
              exit findStrWithZone2
            end if
          end if
        end if
      end do findStrWithZone2
      do i=1,Ner1
        !следующие строки встречаются в массовом файле от BPSD
        read (900,'(i3.3, e14.5)') isot_id, isot_mass
        isot_ids(i) = isot_id
        if (BpsdModeIsots(i).EQ.1) then !тут добавляем обычную массу
          IsotMassSumIn(i) = IsotMassSumIn(i) + isot_mass
        end if
        read (901,'(i3.3, e14.5)') isot_id, isot_mass
        if (BpsdModeIsots(i).EQ.1) then !тут добавляем обычную массу
          IsotMassSumOut(i) = IsotMassSumOut(i) + isot_mass
          MassSumOut = MassSumOut + isot_mass
        end if
        if ((isot_id == 235) .OR. (isot_id == 238) .OR. (isot_id == 236) .OR. (isot_id == 234)) then
          MassDataU(StepGlobal)%mass_recycle_before = MassDataU(StepGlobal)%mass_recycle_before + isot_mass
        end if
        if ((isot_id == 239) .OR. (isot_id == 240) .OR. (isot_id == 241) .OR. (isot_id == 242) .OR. (isot_id == 248) &
        & .OR. (isot_id == 246)) then
          MassDataPu(StepGlobal)%mass_recycle_before = MassDataPu(StepGlobal)%mass_recycle_before + isot_mass
        end if
      end do
      read (900,'(a4,e14.5)') Str16, zone_volume
      VolumeSumOut = VolumeSumOut + zone_volume  !Находим суммарный объем, занимаемый этими изотопами
      RECL_nzon_type(NumZonIn)=5    !теперь можем использовать освободившейся номер зоны
      write (333,*) 'Zone ',NumZonIn,' is empty after recycling. We can use it!'
      close(900)
    end if
    read (901,'(a4,e14.5)') Str16, zone_volume
    read (901,*)
    read (901,*)
    goto 11
  else
    goto 11
  end if
16  close(901)
  end if  !  if (BpsdOn.EQ.1) then







  write (333,*) 'Sum of unloaded mass of all isotopes and all TVS: ',MassSumOut


  !ПЕРАРАБОТКА. ЭТАП 2. Находим массу каждой группы в соотвтетстивии с GroupRecl
  if (.not.allocated(GroupMassSumIn)) allocate(GroupMassSumIn(1:MaxGroupRecl))
  if (.not.allocated(GroupMassSumOut)) allocate(GroupMassSumOut(1:MaxGroupRecl))
  GroupMassSumIn = GroupMassSumIn(1:MaxGroupRecl)
  GroupMassSumOut = GroupMassSumOut(1:MaxGroupRecl)
  GroupMassSumIn = 0
  GroupMassSumOut = 0
  write (777,*) '==========================='
  write (777,*) 'MASSES OF GROUPS, [kg]'
  write (777,*) '------------------'
  do i=1,MaxGroupRecl
    is_first = 1
    write (777,*) 'ID  ISOT     MASS INIT       AFTER RECL'
    do j=1,Ner1
      if (GroupRecl(j) == i) then !если этот изотоп относиться к группе i, то добавляем его массу к массе группы
        GroupMassSumIn(i) = GroupMassSumIn(i) + IsotMassSumIn(j)
        GroupMassSumOut(i) = GroupMassSumOut(i) + IsotMassSumOut(j)
        isot_id = isot_ids(j)
        IsotCapt = GetIsotopeCaption(isot_id)
        if (is_first==1) then
          write (777,'(i3 a2 a4 a e14.5 a e14.5)') I, '  ', IsotCapt,'  ',IsotMassSumIn(j),'  ',IsotMassSumOut(j)
          is_first = 0
        else
          write (777,'(a a4 a e14.5 a e14.5)') '     ', IsotCapt,'  ',IsotMassSumIn(j),'  ',IsotMassSumOut(j)
        end if
      end if
    end do
    write (777,*) '------------------'
    write (777,'(a e14.5 a e14.5)') '    *SUM*  ',GroupMassSumIn(i),'  ',GroupMassSumOut(i)
    write (777,*) '------------------'
    write (777,*)
  end do
  write (777,'(//)')



  !ПЕРАРАБОТКА. ЭТАП 3. Непосредственно перерабатываем каждую группу изотопов в соотвтетсвии со сценарием перегрузок
  ! сценарий при перегрузке каждой группы изотопов: 0 - не трогать изотоп, т.е. что будет после выгрузки, то и оставить;
  !                                                 1 - удалить изотоп после переработки;
  !                                                 2 - сбросить массу изотопа на первоначальный уровень, т.е. добавить/удалить, чтобы масса стала той же, что и была
  if (.not.allocated(GroupMassRecl)) allocate(GroupMassRecl(1:MaxGroupRecl))
  if (.not.allocated(KRecl)) allocate(KRecl(1:MaxGroupRecl))
  KRecl = KRecl(1:MaxGroupRecl)
  GroupMassRecl = GroupMassRecl(1:MaxGroupRecl)
  write (777,*) '==========================='
  write (777,*) 'K OF RECYCLING. Deltas (after minus before) of masses of groups [kg]'
  write (777,*) '------------------'
  write (777,*) ' ID  TYPE   K        MASS BEFORE   MASS AFTER    DELTA'
  n = 0
  MassOfZeroGroupType = 0 !масса тех изотопов, которые не сбрасываются
  MassOf2ndGroupType_OLD = 0 !старая масса тех изотопов, которые мы хотим сбросить на первоначальный уровень
  !Считаем массу тех, которые не трогаем
  do i=1,MaxGroupRecl
    n = TypeRecl(i)
    !0 - не трогаем массу всех изотопов (используем множитель)
    if (n == 0) then
      !подсчитываем суммарную массу изотопов 0го типа переработки ПОСЛЕ ПЕРЕРАБОТКИ
      MassOfZeroGroupType = MassOfZeroGroupType + GroupMassSumOut(i)*Kclear(i)
    end if
    !2 - сбросить массу изотопа на первоначальный уровень, т.е. добавить/удалить, чтобы масса стала той же, что и была
    if (n == 2) then
      !старая масса тех изотопов, которые мы хотим сбросить на первоначальный уровень ПОСЛЕ ПЕРЕРАБОТКИ
      MassOf2ndGroupType_OLD = MassOf2ndGroupType_OLD + GroupMassSumOut(i)*Kclear(i)
    end if
  end do
  !общая масса, которую нужно добавить ко второй группе, чтобы масса ТВС стала первоначальной
  MassOf2ndGroupType = MassSumOut - MassOfZeroGroupType - MassOf2ndGroupType_OLD
  do i=1,MaxGroupRecl
    n = TypeRecl(i)
    !0 - не трогаем массу всех изотопов (используем множитель)
    if (n == 0) then
      GroupMassRecl(i) = GroupMassSumOut(i)*Kclear(i)
    end if
    !NOT RECCOMENDED TO USE THIS !!!! !1 - удалить изотоп после переработки (оставляем долю из Kclear)
    if (n == 1) GroupMassRecl(i) = 0
    !2 - сбросить массу изотопа на первоначальный уровень, т.е. добавить/удалить, чтобы масса стала той же, что и была
    if (n == 2) then
!      GroupMassRecl(i) = MassOf2ndGroupType*GroupMassSumIn(i)/MassOf2ndGroupType_OLD  !масса каждого изотопа - это общая масса умножить на долю этого изотопа изначально
       GroupMassRecl(i) = GroupMassSumOut(i)*Kclear(i)
    end if
    if (GroupMassSumOut(i) > 0) then
      KRecl(i) = GroupMassRecl(i)/GroupMassSumOut(i)     !Ищем отношения конечн. начальное для каждой группы изотопов
    else
      KRecl(i) = 0 !если массы нет, то и суда нет)) - такое бывает при использовании BPSD
    end if
    delta_mass = GroupMassRecl(i)-GroupMassSumIn(i)    !Сколько было добавлено, удалено массы на этом шаге
    write (777,'(i4,i4,f11.4,3e14.5)') i, n, KRecl(i), GroupMassRecl(i), GroupMassSumOut(i), delta_mass
  end do
  write(333, *) 'Mass of constant materials for TypeRecl=0, MassOfZeroGroupType = ',MassOfZeroGroupType
  write(333, *) 'Mass of unloaded materials for TypeRecl=2, MassOf2ndGroupType_OLD = ',MassOf2ndGroupType_OLD
  write(333, *) 'Mass that to be added for fullfil Assembly to it previous mass '// &
  &'(for TypeRecl=2), MassOf2ndGroupType = ',MassOf2ndGroupType
  write (777,'(//)')



  !ПЕРАРАБОТКА. ЭТАП 4. Наконец, находим новую концентрацию каждого изотопа после переработки (она одинаковая для всех зон)
  if (.not.allocated(IsotReclMass)) allocate(IsotReclMass(1:Ner1))
  if (.not.allocated(IsotReclConc)) allocate(IsotReclConc(1:Ner1))
  IsotReclMass = IsotReclMass(1:Ner1)
  IsotReclConc = IsotReclConc(1:Ner1)
  write (777,*) '==========================='
  write (777,*) 'ISOTOPES DETAILS DATA'
  write (777,*) 'New masses of isotopes after recycling and its concentrations. Deltas (after minus before) of masses of isotopes'
  write (777,*) 'ID   NAME   GR  MASS BEFORE   MASS AFTER    DELTA         RECL CONC'
  n = 0
  do i=1,Ner1
    !ищем, к какой группе принадележит изотоп
    n = GroupRecl(i)
    !умножаем коэффициенты очистки на каждый изотоп (коэффициенты мы задали на шаге №3 в соответствии со входным файлом)
    !добавляем недостающую массу для TypeRecl=2 в соответствии с долями изотопов в общей массе этих элементов
    IsotReclMass(i) = KRecl(n)*IsotMassSumOut(i) + GrReclFraq(i)*MassOf2ndGroupType
    !Получаем идентификатор текущего изотопа
    isot_id = isot_ids(i)
    !Считаем атомную массу текущего изотопа
    atomic_mass = GetIsotopeAtomicMass(isot_id)
    !находим концентрации из массы текущего изотопа
    IsotReclConc(i) = IsotReclMass(i)*CONST_NA/(0.001*Kvol*CONST_barn*atomic_mass*VolumeSumOut)
    IsotCapt = GetIsotopeCaption(isot_id)
    delta_mass = IsotReclMass(i)-IsotMassSumIn(i)
    write (777,'(i4,a6,i4,5e14.5)') isot_id, IsotCapt, n, IsotMassSumIn(i), IsotReclMass(i), delta_mass, IsotReclConc(i)
    if ((isot_id == 235) .OR. (isot_id == 238) .OR. (isot_id == 236) .OR. (isot_id == 234)) then
      MassDataU(StepGlobal)%mass_recycle_after = MassDataU(StepGlobal)%mass_recycle_after + IsotReclMass(i)
    end if
    if ((isot_id == 239) .OR. (isot_id == 240) .OR. (isot_id == 241) .OR. (isot_id == 242) .OR. (isot_id == 248) &
     & .OR. (isot_id == 246)) then
      MassDataPu(StepGlobal)%mass_recycle_after = MassDataPu(StepGlobal)%mass_recycle_after + IsotReclMass(i)
    end if
  end do
  write (777,'(//)')
  close(777)  !закрываем файл с данными по переработке



  !унчичтожаем временные массивы для обработки данных перед выхдом
  deallocate(isot_ids)
  deallocate(IsotMassSumIn)
  deallocate(IsotMassSumOut)
  deallocate(GroupMassSumIn)
  deallocate(GroupMassSumOut)
  deallocate(IsotReclMass)
  deallocate(GroupMassRecl)
  deallocate(KRecl)
end subroutine DoRecycle
