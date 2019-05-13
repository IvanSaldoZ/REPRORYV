!ФОРМИРУЕМ НОВЫЙ ВХОДНОЙ ФАЙЛ ДЛЯ JARFR ПРИ НУЛЕВОМ ЗАПУСКЕ ПРОГРАММЫ (ЗАПУСКАЕМ НА ПРОМЕЖУТОК ВРЕМЕНИ НАЧАЛЬНЫХ СУТОК)
subroutine FormNewInputJARFRFile
    use CommonModJARFR
    !счетчик новых ТВСок и новых УНИКАЛЬНЫХ ТВСок (т.е. тех номеров, которые ещё не были использованы)
    integer AddNewTVSType, AddNewTVSUniqueType
    !счетчик общего количества и уникальных номеров ТВСок, выгруженных на предыдущем шаге
    integer MinusOneTVSCount
    !То количество новых материалов (физических зон) и их уникальных номеров, которое мы должны добавить для фиксации выгуржаемых материалов в конце шага
    integer AddMatsCount, AddMatsUniqueCount
    !То количество новых и уникальных материалов (физических зон), которое мы должны добавить вместо выгруженных материалов на прыдыдущем шаге
    integer AddMatsPreCount, AddMatsPreUniqueCount
    !Новое/старое кол-во зон
    integer NewMatsCount, OldMatsCount
    !Массив номеров ТВС, которые мы хотим выгрузить, массив новых номеров ТВС
    integer, allocatable :: RECL_TVSnumToReload(:), RECL_TVSnumToReload_TEMP(:), RECL_TVSnumCaption(:)
    !Массив номеров ТВС, которые мы были выгружены на преыдудущем шаге
    integer, allocatable :: RECL_TVSnumUnloaded(:), RECL_TVSnumUnloaded_TEMP(:)
    !Массив номеров составов, которые мы хотим выгрузить
    integer, allocatable :: RECL_FIZids(:), RECL_FIZids_BIG(:), RECL_FIZids_TEMP(:)
    !Массив номеров составов, которые соответствуют тем, которые мы хотим выгрузить (соответствие между RECL_FIZids <-> RECL_FizNewIds)
    integer, allocatable :: RECL_FizNewIds(:)
    !Массив номеров составов, которые были выгружены на предыдущем шаге
    integer, allocatable :: RECL_FIZolds(:), RECL_FIZolds_TEMP(:), RECL_FIZolds_BIG(:)
    !Массив номеров составов, которые соответствуют тем, которые были выгружены (соответствие между RECL_FIZolds <-> RECL_FizNewOld)
    integer, allocatable :: RECL_FizNewPre(:)
    !есть ли в массиве такой элемент
    logical isInArr
    !Двухмерный массив KT
    integer, allocatable :: KTx2(:,:), KTx2_ORIGINAL(:,:), KTx2_TEMP(:,:)
    !Индекс, в который копируем номера составов или сами составы
    integer newIndex
    !Двухмерный массив AD - концентраций каждого из номеров составов
    real, allocatable :: ADx2(:,:)
    !Есть ли свободный неиспользуемый номер ТВСки для повтороного использования
    logical isFreeToUse
    !Новый индекс ТВСки/физической зоны для повторного использования
    integer newTVSindex, newFIZindex
    !Строка для формата вывода массивов NK и KT
    character*50 :: FMT2, FMT3
    !Имеется ли данная зона в активной зоне или её можно использовать
    integer, allocatable :: FizZoneStatus_TEMP(:)
    !Для чтения файла
    character*11 Str11
    character*250 Str250
    !Имя файла с начальными концентрациями
    character*250 curFile_CONC
    !Номер читаемой зоны
    integer NumZonIn, NumZonFound
    !Внутренний идентификатор изотопа
    integer IsotID
    !Считанная концентрация
    real ConcIn
    !Перед удалением делаем копию массива Tem
    real, allocatable :: Tem_Copy(:)
    !Временная копия массива Ntnz
    integer, allocatable :: Ntnz_temp(:)
    !Строка для обработки файлов
    character*6 AddedStr, AddEndOfOutputFile

    re_i = system('copy '//inp_new//' '//trim(inp_new_to_run)//'  1>>tmp 2>&1')
    re_i = system(trim(DelComm)//' '//inp_new//'  1>>tmp 2>&1')
    open(unt1, file = trim(inp_new_to_run), status = 'OLD')
    !Включаем режим выгорания топлива
    Jupbn = 1
    !Время на переработку должно быть  согласовано с временем на одну микрокампанию работы топлива в реакторе
    T1Recl = TCamp + TDown
    !Общее кол-во времени облучения/выдержки/переработки
    T=(TCamp + TDown + (nVRH+nRecl)*T1Recl)
    !На сколько делим интервал
    !NDTnum = 1  !кол-во интервалов, на которые мы разбиваем время работы реактора (чтобы в этих точках получить Keff)
    NDT = NDTnum + 2 !5 интервалов на мощности и 2 - для получения данных по выдержке и переработке
    !Выделяем память для массивов
    deallocate(Jprn)
    deallocate(Jdpt)
    deallocate(T_Usr)
    deallocate(PWM)
    allocate(Jprn(1:NDT))
    allocate(Jdpt(1:NDT))
    allocate(T_Usr(1:NDT))
    allocate(PWM(1:NDT-1))
    !Время по шагам
    do i=1,NDTnum !назначаем каждый интервал времени
      T_Usr(i) = TCamp/NDTnum  !Время работы на мощности (330 сут/кол-во интервалов)
      if (i < NDTnum) then
        PWM(i) = Pw  !Мощность берем из заданной
      end if
    end do
    !T_Usr(1) = TCamp  !Время работы на мощности (330 сут)
    T_Usr(NDTnum + 1) = TDown  !Время выдержки (30 сут)
    T_Usr(NDTnum + 2) = (nVRH+nRecl)*T1Recl  !Время выдержки + переработки (2+1 год) во внутриреакторном хранилище
    !Мощность по шагам
    PWM(NDTnum) = 0.0000001  !Мощность выдержки (30 сут)
    PWM(NDTnum+1) = 0.0000001  !Мощность выдержки + переработки (2+1 год) во внутриреакторном хранилище
    Jprn = 3
    Jpri = 2 !Нужно для отображения микросечений
    JDPT = 0
    NKR = 1  !Нужно для нахождения Kr
    NKV = 1  !Нужно для нахождения Коэффициента воспроизводства

    !===============================
    !ТЕПЕРЬ САМОЕ СЛОЖНОЕ!!!!
    !===============================
    !ОБРАБОТКА МАССИВОВ KT И NK
    !НАХОДИМ, КАКИЕ НОМЕРА ТВС НАМ НУЖНО ПЕРЕГРУЗИТЬ
    i = 1
    AddNewTVSType = 1
    AddNewTVSUniqueType = 1
    allocate(RECL_TVSnumToReload_TEMP(1:99999)) !выделяем начальную память
    RECL_TVSnumToReload_TEMP = -1
    !Находим, номера типов ТВС, которые нужно выгрузить из АЗ в конце микрокампании
    do while (i <= NkDimention)
      !для каждого элемента из списка перегрузок находим текущий год перегрузки
      !if (isTEST >= 2) write(333, *) 'NkRecl(',i,')=',NkRecl(i);
      !если это текущий год, то:
!      if (NkRecl(i) > 0) then
      if (NkRecl(i) == MkCampCounter) then
        !сначала проверяем есть ли уже такой номер типа ТВС в массиве
        isInArr = .false.
        j = 1
        do while (j <= AddNewTVSType)
          if (RECL_TVSnumToReload_TEMP(j) == Nk(i)) then
            isInArr = .true.
          end if
          j = j + 1
        end do
        !если в массиве ТВСок данного типа нет, то добавляем
        if (isInArr.eqv..false.) then
          !RECL_TVSnumToReload = RECL_TVSnumToReload(1:AddNewTVSType)
          RECL_TVSnumToReload_TEMP(AddNewTVSType) = Nk(i)
          !if (isTEST >= 2) write(333, *) 'RECL_TVSnumToReload(',AddNewTVSType,')=',RECL_TVSnumToReload(AddNewTVSType)
          AddNewTVSType = AddNewTVSType + 1
        end if
      end if
      i = i + 1
    enddo
    !возвращаем кол-во добавленных номеров ТВС на место
    AddNewTVSType = AddNewTVSType - 1
    allocate(RECL_TVSnumToReload(1:AddNewTVSType))
    do i=1,AddNewTVSType
      RECL_TVSnumToReload(i)=RECL_TVSnumToReload_TEMP(i)
    end do
    deallocate(RECL_TVSnumToReload_TEMP) !удаляем временный массив из памяти
    AddNewTVSUniqueType = AddNewTVSType !(пока что они равны)
    !Сортируем массив RECL_TVSnumToReload (чтобы по порядку было все)
    call sort(RECL_TVSnumToReload, AddNewTVSType)
    write(333, *) 'Added new TVS on this step=',AddNewTVSType


    !НАХОДИМ, КАКИЕ ТВС БЫЛИ ВЫГРУЖЕНЫ НА ПРЕДЫДУЩЕМ ШАГЕ И ИСПОЛЬЗУЕМ ИХ НОМЕРА ДЛЯ ЗАГРУЗКИ НОВЫХ/ПЕРЕРАБОТАННЫХ ТВСОК
    if (StepGlobal > 1) then
      i = 1
      MinusOneTVSCount = 1  !количество ТВСок, выгруженных на предыдущем шаге
      !MinusOneTVSUniqueCount = 1  !количество уникальных новых номеров ТВСок, выгруженных на предыдущем шаге
      allocate(RECL_TVSnumUnloaded_TEMP(1:999999))
      RECL_TVSnumUnloaded_TEMP = -1 !инициализируем
      !Находим, номера типов ТВС, которые были выгружены из АЗ в конце предыдущей микрокампании
      do i=1,NkDimention
        !для каждого элемента из списка перегрузок находим предыдущий год перегрузки (микрокампании)
        if (MkCampCounter == 1) then
          j = MkCamp
        else
          j = MkCampCounter - 1
        end if
        if (NkRecl(i) == j) then
          !сначала проверяем есть ли уже такой номер типа ТВС в массиве
          isInArr = .false.
          do j = 1,MinusOneTVSCount
            if (RECL_TVSnumUnloaded_TEMP(j) == Nk(i)) then
              isInArr = .true.
            end if
          end do
          !если в массиве ТВСок данного типа нет, то добавляем
          if (isInArr.eqv..false.) then
            !RECL_TVSnumUnloaded = RECL_TVSnumUnloaded(1:MinusOneTVSCount)
            RECL_TVSnumUnloaded_TEMP(MinusOneTVSCount) = Nk(i)
            !if (isTEST >= 2) write(333, *) 'RECL_TVSnumUnloaded(',MinusOneTVSCount,')=',RECL_TVSnumUnloaded(MinusOneTVSCount)
            MinusOneTVSCount = MinusOneTVSCount + 1
          end if
        end if
      end do
      !возвращаем кол-во добавленных номеров ТВС на место
      MinusOneTVSCount = MinusOneTVSCount - 1
      !AddNewTVSUniqueType = MinusOneTVSCount   !(пока что они равны)
      !MinusOneTVSUniqueCount = MinusOneTVSCount   !(пока что они равны)
      allocate(RECL_TVSnumUnloaded(1:MinusOneTVSCount))  !выделяем начальную память
      do i=1,MinusOneTVSCount
        RECL_TVSnumUnloaded(i) = RECL_TVSnumUnloaded_TEMP(i)
      end do
      deallocate(RECL_TVSnumUnloaded_TEMP)
      !Сортируем массив RECL_TVSnumToReload (чтобы по порядку было все)
      call sort(RECL_TVSnumUnloaded, MinusOneTVSCount)
      write(333, *) 'Old TVS number from previous step=',MinusOneTVSCount
      write(333, *) 'TVS from previous step that were unloaded(RECL_TVSnumUnloaded): ',RECL_TVSnumUnloaded
      if (isTEST >= 2) write(333, *) 'RECL_TVSnumUnloaded=',RECL_TVSnumUnloaded
    end if


    !ПРОВЕРЯЕМ НАЛИЧИЕ ИЛИ ОТСУТСТВИЕ ТЕХ ИЛИ ИНЫХ НОМЕРОВ ТВС/ФИЗИЧЕСКИХ ЗОН В АКТИВНОЙ ЗОНЕ ПОСЛЕ ВЫГРУЗКИ ТВСОК НА ЭТОМ ШАГЕ И ВЫКЛЮЧАЕМ (СТАВИМ НОЛЬ) СООТСУТСТВУЮЩИЕ НОМЕРА, ЕСЛИ ЭТИ НОМЕРА НЕ ИСПОЛЬЗУЮТСЯ
    !Инициализируем переменные
    deallocate(RECL_TVSnum_Enabled)
    allocate(RECL_TVSnum_Enabled(1:maxNk+AddNewTVSUniqueType))
    write(333, *) 'CHECKING FOR THE EXISTANCE OF THE TVS IN THE CORE...'
    !if (isTEST >= 2) write(333, *) 'CHECKING FOR THE EXISTANCE OF THE FIZ ZONES IN THE CORE'
    RECL_TVSnum_Enabled = 1
    do i=1,AddNewTVSType !для каждого нового типа ТВС
      !do k=1,MkCamp
        newTVSindex = RECL_TVSnumToReload(i)
        !if (isTEST >= 2) write(333, *) 'newTVSindex=',newTVSindex
        isInArr = .false.
        isTVSExists: do j=1,NkDimention
         ! if (isTEST >= 2) write(333, *) 'Nk(',j,')=',Nk(j)
          if (Nk(j) == newTVSindex) then
            !Исключаем те ТВС, которые мы хотим вытащить на этом шаге (это нужно для того, чтобы искать выгружаемы ТВС среди только других ТВСок)
!            if (NkRecl(j).ne.k) then
            if (NkRecl(j).ne.MkCampCounter) then
              isInArr = .true. !такая ТВСка уже есть в массиве
            end if
            !EXIT isTVSExists !выход из цикла
          end if
        end do isTVSExists
        !если же такого типа ТВС не оказалось в массиве, то нужно проверить, есть ли там такие физ-зоны, которые больше нигде не встречаются (чтобы занулить их концентрации, если они больше не используются)
        if (isInArr.eqv..false.) then
          !проверяем, занесен ли уже этот номер в список отсутствующих ТВСок
          RECL_TVSnum_Enabled(newTVSindex)=0
          !if (isTEST >= 2) write(333, *) 'TVS TYPE IS NOT FOUND IN ARRAY, finding the existance of fiz zones in this TVS'
          write(333, *) 'TVS TYPE',newTVSindex,' IS NOT FOUND IN ARRAY. Disable it.'
        end if
      !end do
    end do


    !Добавляем массив индексов для новых типов ТВС
    allocate(RECL_TVSnumCaption(1:AddNewTVSType)) !выделяем начальную память для массива новых номеров ТВС
    !RECL_TVSnumCaption=RECL_TVSnumCaption(1:AddNewTVSType) !выделяем начальную память для массива новых номеров ТВС
    i = 1
    n = 0
    do while (i <= AddNewTVSType)
      isFreeToUse = .false.
      newTVSindex = -1
      !пытаемся найти НЕ занятый номер ТВСки, чтобы использовать его для обозначения новой загрузки (для начала только после 1-й кампании реактора)
      FindFreeTVS: do j=1,maxNk
        if (RECL_TVSnum_Enabled(j)==0) then
          isFreeToUse = .true.
          write(333, *) 'TVS TYPE',j,' IS USED FOR NEW MATERIALS. ENABLE IT'
          newTVSindex = j
          RECL_TVSnum_Enabled(j) = 1
          exit FindFreeTVS
        end if
      end do FindFreeTVS
      !теперь вместо того, чтобы использовать новый номер ТВСки, заменяем его на старый номер
      if (isFreeToUse.eqv..false.) then
        RECL_TVSnumCaption(i) = maxNk + i - n
        !write(333,*) 'RECL_TVSnumCaption(i)=',RECL_TVSnumCaption(i)
      else
        RECL_TVSnumCaption(i) = newTVSindex
        AddNewTVSUniqueType = AddNewTVSUniqueType - 1 !если мы используем уже старый номер, то уникальных номеров у нас убавилось
        n = n + 1
      end if
      i = i + 1
    end do
    write(333, *) 'Added unique type of TVS: ',AddNewTVSUniqueType
    write(333, *) 'NEW TVS CAPTION:',RECL_TVSnumCaption
    !ОТОБРАЖАЕМ СОДЕРЖИМОЕ МАССИВА NK В УДОБНОМ ВИДЕ - ДО ЗАМЕНЫ
    n = 1
    write(333, *) '----'
    write(333, *) 'Nk - TVS MAP BEFORE'
    do i=1,Nr1
      FMT2 = ''
      write(FMT2,*) Ns(i)
      FMT3 = ''
      FMT3 = '('//trim(FMT2)//'i4)'
      write(333, FMT3) Nk((i-1)*Ns(i)+1:i*Ns(i))
      do j=1,Ns(i)
        n = n + 1
      end do
    end do


    !ЗАМЕНЯЕМ В NK НОМЕРА ТВС НА ТЕ, КОТОРЫЕ БУДУТ ВЫГРУЖЕНЫ В КОНЦЕ ЭТОГО ШАГА ДЛЯ ПЕРЕРАБОТКИ
    do i=1,NkDimention
    !ПОПЫТКА ЗАМЕНИТЬ СРАЗУ ВСЕ ТВСКИ
      !do k=1,MkCamp
        !для каждого элемента из списка перегрузок находим текущий год перегрузки
        !if (NkRecl(i) == k) then
         if (NkRecl(i) == MkCampCounter) then
          do j=1,AddNewTVSType
            !находим номер массива, соответстующий нашему типу ТВС
            if (RECL_TVSnumToReload(j) == Nk(i)) then
              Nk(i) = RECL_TVSnumCaption(j)
            end if
          end do
        end if
      !end do
    end do

    !ОТОБРАЖАЕМ СОДЕРЖИМОЕ МАССИВА NK В УДОБНОМ ВИДЕ
    n = 1
!    if (isTEST >= 2) write(333, *) '----'
!    if (isTEST >= 2) write(333, *) 'Nk - TVS NEW MAP'
    write(333, *) '----'
    write(333, *) 'Nk - TVS NEW MAP'
    do i=1,Nr1
      write(FMT2,*) Ns(i)
      FMT2 = '('//trim(FMT2)//'i4)'
      write(333, FMT2) Nk((i-1)*Ns(i)+1:i*Ns(i))
      do j=1,Ns(i)
        n = n + 1
      end do
    end do

    !ДЕЛАЕМ ДВУМЕРНЫЙ МАССИВ ИЗ KT, ЧТОБЫ ЛЕГЧЕ БЫЛО С НИМ ОБРАЩАТЬСЯ
    k = 1 !увеличиваем номера для неделящихся материалов
    allocate(KTx2(1:maxNk, 1:MaxNSLZ))
    allocate(KTx2_ORIGINAL(1:maxNk, 1:MaxNSLZ)) !Сохраняем оригинал для сравнения
    do i=1,maxNk
      do j=1,MaxNSLZ
        KTx2(i,j) = KT(k) !заполняем двухмерный массив
        KTx2_ORIGINAL(i,j) = KT(k) !сохраняем оригинал на всякий
        k = k + 1
      end do
    end do

    !ОТОБРАЖАЕМ СОДЕРЖИМОЕ МАССИВА KT В УДОБНОМ ВИДЕ - ИСХОДНЫЙ МАССИВ
    write(333, *) '----'
    write(333, *) 'KT - ZONES ORIGINAL'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk
      write(333, trim(FMT2)) KT((i-1)*MaxNSLZ+1:i*MaxNSLZ)
    end do


    !ГЕНЕРИРУЕМ НОВЫЕ НОМЕРА СОСТАВОВ ДЛЯ ПЕРЕГРУЗКИ НА ЭТОМ ШАГЕ

    !ТЕПЕРЬ НАХОДИМ В KT ТЕ НОМЕРА СОСТАВОВ, КОТОРЫЕ НАМ НУЖНО ПЕРЕГРУЗИТЬ И ЗАНОСИМ ИХ В МАССИВ RECL_FIZids_TEMP
    write(333, *) '----'
    write(333, *) 'Finding in KT the fiz zones that need to be recycled on this step...'
    write(333, *) 'TVS that will be reloaded on this step (RECL_TVSnumToReload): ',RECL_TVSnumToReload
!   if (isTEST >= 2) write(333, *) 'RECL_TVSnumToReload= ',RECL_TVSnumToReload
    k = 1 !число перегружаемых материалов
    allocate(RECL_FIZids_TEMP(1:99999))
    RECL_FIZids_TEMP = -1
    do j=1,AddNewTVSType
      do i=1,maxNk
        !проверяем только те строки, которые нам нужны (= номеру ТВСки)
        if (i == RECL_TVSnumToReload(j)) then
          do m=1,MaxNSLZ
            if (KTx2(i,m) <= Nf) then
              !RECL_FIZids_TEMP=RECL_FIZids_TEMP(1:k)
              RECL_FIZids_TEMP(k) = KTx2(i,m)
              k = k + 1
            end if
          end do
        end if
      end do
    end do

    !RECL_FIZids_TEMP = RECL_FIZids_TEMP(1:k-1)
    if (isTEST >= 2) write(333, *) 'RECL_FIZids_TEMP= ',RECL_FIZids_TEMP(1:k-1)
    !нам нужны только уникальные составы, поэтому из массива TEMP оставляем только уникальные номера
    allocate(RECL_FIZids_BIG(1:99999))
    RECL_FIZids_BIG = -1
    n = 1
    do i=1,k-1
      !Смотрим, есть ли такой состав в массиве
      isInArr =.false.
      do j=1,n
        if (RECL_FIZids_BIG(j)==RECL_FIZids_TEMP(i)) then
          isInArr =.true.
        end if
      end do
      if (isInArr.eqv..false.) then !если такого состава ещё нет в массиве, то добавляем
        RECL_FIZids_BIG(n) = RECL_FIZids_TEMP(i)
        n = n + 1
      end if
    end do
    AddMatsCount = n - 1   !количество новых типов материалов
    allocate(RECL_FIZids(1:AddMatsCount))
    do i=1,AddMatsCount
      RECL_FIZids(i) = RECL_FIZids_BIG(i)
    end do
    deallocate(RECL_FIZids_BIG)
    !RECL_FIZids=RECL_FIZids(1:AddMatsCount)
    call sort(RECL_FIZids, AddMatsCount)
    if (isTEST >= 2) write(333, *) 'RECL_FIZids= ',RECL_FIZids
    AddMatsUniqueCount = AddMatsCount !на начальный момент времени они равны
    write(333, *) '-------'
    write(333, *) 'Added new materials count for recycling (AddMatsCount)=',AddMatsCount
    write(333, *) 'Founded materials that need to be recycled on this step (RECL_FIZids)=',RECL_FIZids
    !ЗАПОЛНЯЕМ МАССИВ НОМЕРОВ НОВЫХ СОСТАВОВ, СООТВЕТСТВУЮЩИХ ВЫГРУЖАЕМЫМ, ЧТОБЫ МОЖНО БЫЛО ОТДЕЛЬНО ЗА НИМИ ПОТОМ СЛЕДИТЬ
    write(333, *) 'Generate new numbers of fiz zones for the recycling...'
    allocate(RECL_FizNewIds(1:AddMatsCount)) !выделяем память для массива, в котором будут храниться наши номера новых составов
    n = 1
    do i=1,AddMatsCount
      !ищем свободные номера
      isFreeToUse = .false.
      newFIZindex = -1
      FindFreeFiz: do j=1,M1
        if (RECL_nzon_type(j)==5) then !если нашли свободный номер, то занимаем его
        !if (RECL_nzon_type(j)==444) then !если нашли свободный номер, то занимаем его
          newFIZindex = j
          write(333, *) 'Founded not used material',j,'. Use it for farther recycling.'
          isFreeToUse = .true.
          RECL_nzon_type(j) = 2   !на следующем шаге эта зона будет переработана
          exit FindFreeFiz
        end if
      end do FindFreeFiz
      !если свободных номеров нет, то добавляем новый материал
      if (isFreeToUse.eqv..false.) then
        RECL_FizNewIds(i) = Nf + n
        n = n + 1
      else !если свободные номера есть - то занимаем его
        RECL_FizNewIds(i) = newFIZindex
        AddMatsUniqueCount = AddMatsUniqueCount - 1 !реально новых материалов теперь стало на единицу меньше
      end if
    end do
    !СТАВИМ СТАТУС НОВЫХ ЗОН НА 2-КУ - НА КОНЕЦ ЭТОГО ШАГА ДАННЫЕ ЗОНЫ БУДУТ ПЕРЕРАБОТАНЫ

    allocate(RECL_nzon_type_temp(1:M1))
    RECL_nzon_type_temp=RECL_nzon_type
    deallocate(RECL_nzon_type)
    allocate(RECL_nzon_type(1:M1+AddMatsUniqueCount))
    do i=1,M1
      RECL_nzon_type(i)=RECL_nzon_type_temp(i)
    end do
    deallocate(RECL_nzon_type_temp)
    !RECL_nzon_type = RECL_nzon_type(1:M1+AddMatsUniqueCount)
    do i=1,AddMatsCount
      j = RECL_FizNewIds(i)
      RECL_nzon_type(j) = 2   !на следующем шаге эта зона будет переработана
    end do
    !if (isTEST >= 2) write(333, *) 'RECL_FizNewIds= ',RECL_FizNewIds
    !if (isTEST >= 2) write(333, *) 'AddMatsUniqueCount= ',AddMatsUniqueCount
    !Для новых материалов включаем их статус
    !RECL_Fiz_Enabled = RECL_Fiz_Enabled(1:(M1+AddMatsUniqueCount))
    !Сохраняем шаг, на котором были загружены данные зоны
    !RECL_FizStepLoaded = RECL_FizStepLoaded(1:(M1+AddMatsUniqueCount))
    RECL_FizStepLoaded((M1+1):(M1+AddMatsUniqueCount)) = StepGlobal
    do i=1,(M1+AddMatsUniqueCount)
      do j=1,AddMatsCount
        if (i == RECL_FizNewIds(j)) then
!          RECL_FizStepLoaded(i) = StepGlobal
        end if
      end do
    end do

    !ГЕНЕРИРУЕМ НОВЫЕ НОМЕРА ДЛЯ ВЫГРУЖЕННЫХ НОМЕРОВ СОСТАВОВ
    AddMatsPreCount = 0
    AddMatsPreUniqueCount = 0
    if (StepGlobal > 1) then
      !НАХОДИМ В KT ТЕ НОМЕРА СОСТАВОВ, КОТОРЫЕ БЫЛИ ВЫГРУЖЕНЫ НА ПРЕДЫДУЩЕМ ШАГЕ И ЗАНОСИМ ИХ В МАССИВ RECL_FIZolds_TEMP
      write(333, *)
      write(333, *) '----'
      write(333, *) 'Finding in KT the fiz zones that were unloaded on the previous step...'
      k = 1 !число перегружаемых материалов
      allocate(RECL_FIZolds_TEMP(1:99999))
      RECL_FIZolds_TEMP = -1
      !allocate(RECL_FIZolds_TEMP(1:k))
      !do m=1,AddNewTVSType
      do m=1,MinusOneTVSCount
        do i=1,maxNk
          !проверяем только те строки, которые нам нужны (= номеру ТВСки)
          if (i == RECL_TVSnumUnloaded(m)) then
            do j=1,MaxNSLZ
              if (KTx2(i,j) <= Nf) then
               ! RECL_FIZolds_TEMP=RECL_FIZolds_TEMP(1:k)
                RECL_FIZolds_TEMP(k) = KTx2(i,j)
                k = k + 1
              end if
            end do
          end if
        end do
      end do
      if (isTEST >= 2) write(333, *) 'RECL_FIZolds_TEMP= ',RECL_FIZolds_TEMP(1:k-1)
      !нам нужны только уникальные составы, поэтому из массива TEMP оставляем только уникальные номера
      n = 1
      allocate(RECL_FIZolds_BIG(1:99999))
      RECL_FIZolds_BIG = -1
      do i=1,k-1
        !Смотрим, есть ли такой состав в массиве
        isInArr =.false.
        do j=1,n
          if (RECL_FIZolds_BIG(j)==RECL_FIZolds_TEMP(i)) then
            isInArr =.true.
          end if
        end do
        if (isInArr.eqv..false.) then !если такого состава ещё нет в массиве, то добавляем
          RECL_FIZolds_BIG(n) = RECL_FIZolds_TEMP(i)
          n = n + 1
        end if
      end do
      AddMatsPreCount = n - 1 !количество новых типов материалов для загрузки в них новых концентраций
      allocate(RECL_FIZolds(1:AddMatsPreCount))
      do i=1,AddMatsPreCount
        RECL_FIZolds(i) = RECL_FIZolds_BIG(i)
      end do
      deallocate(RECL_FIZolds_BIG)
      !RECL_FIZolds=RECL_FIZolds(1:AddMatsPreCount)
      call sort(RECL_FIZolds, AddMatsPreCount)
      if (isTEST >= 2) write(333, *) 'RECL_FIZolds= ',RECL_FIZolds
      AddMatsPreUniqueCount = AddMatsPreCount !на начальный момент времени они равны
      write(333, *) 'Added new materials count for previously unloaded (AddMatsPreCount)=',AddMatsPreCount
      write(333, *) 'Founded materials that were unloaded on previously step (RECL_FIZolds)=',RECL_FIZolds
      !ВЫКЛЮЧАЕМ ДЛЯ ОТОБРАЖЕНИЯ ИЗВЛЕЧЕННЫЕ НОМЕРА ЗОН
      do i=1,AddMatsPreCount
        j = RECL_FIZolds(i)
        RECL_nzon_type(j) = 0
      end do
      !ЗАПОЛНЯЕМ МАССИВ НОМЕРОВ НОВЫХ СОСТАВОВ, СООТВЕТСТВУЮЩИХ ВЫГРУЖАЕМЫМ, ЧТОБЫ МОЖНО БЫЛО ОТДЕЛЬНО ЗА НИМИ ПОТОМ СЛЕДИТЬ
      write(333, *) 'Generate new numbers of fiz zones for the previously unloaded materials...'
      allocate(RECL_FizNewPre(1:AddMatsPreCount)) !выделяем память для массива, в котором будут храниться наши номера новых составов
      n = 1
      do i=1,AddMatsPreCount
        !ищем свободные номера
        isFreeToUse = .false.
        newFIZindex = -1
        FindFreeFizPre: do j=1,M1
          if (RECL_nzon_type(j)==5) then !если нашли свободный номер, то занимаем его
          !if (RECL_nzon_type(j)==444) then !если нашли свободный номер, то занимаем его
            newFIZindex = j
            write(333, *) 'Founded not used material',j,'. Use it for loading new zone.'
            isFreeToUse = .true.
            RECL_nzon_type(j) = 1   !просто обычная новая зона
            exit FindFreeFizPre
          end if
        end do FindFreeFizPre
        !если свободных номеров нет, то добавляем новый материал
        if (isFreeToUse.eqv..false.) then
          RECL_FizNewPre(i) = Nf + n     + AddMatsUniqueCount  !С УЧЕТОМ ТОГО, ЧТО МЫ УЖЕ ДОБАВИЛИ МАТЕРИАЛЫ ДЛЯ РЕЦИКЛА
          n = n + 1
        else !если свободные номера есть - то занимаем его
          RECL_FizNewPre(i) = newFIZindex
          AddMatsPreUniqueCount = AddMatsPreUniqueCount - 1 !реально новых материалов теперь стало на единицу меньше
        end if
      end do
      !СТАВИМ СТАТУС НОВЫХ ЗОН НА 1-КУ - обычная новая зона
      allocate(RECL_nzon_type_temp(1:M1+AddMatsUniqueCount))
      RECL_nzon_type_temp=RECL_nzon_type
      deallocate(RECL_nzon_type)
      allocate(RECL_nzon_type(1:M1+AddMatsUniqueCount+AddMatsPreUniqueCount))
      do i=1,M1+AddMatsUniqueCount
        RECL_nzon_type(i)=RECL_nzon_type_temp(i)
      end do
      deallocate(RECL_nzon_type_temp)
      do i=1,AddMatsPreCount
        j = RECL_FizNewPre(i)
        RECL_nzon_type(j) = 1   !просто обычная новая зона
      end do
      !if (isTEST >= 2) write(333, *) 'RECL_FizNewPre= ',RECL_FizNewPre
      !if (isTEST >= 2) write(333, *) 'AddMatsPreUniqueCount= ',AddMatsPreUniqueCount
      write(333, *) 'New captions of new materials for unloaded previously zones (RECL_FizNewPre): ',RECL_FizNewPre
      write(333, *) 'New UNIQUE material count for unloaded previously zones (AddMatsPreUniqueCount): ',AddMatsPreUniqueCount
    end if
    if (StepGlobal > 1) then
      !Добавляем только что добавленные новые и старые номера выгруженных зон в специальный глобальный массив
      do i=1,AddMatsPreCount
        !Сохраняем в глобальные переменные, чтобы потом можно было воспользоваться при перегрузках
        RECL_FizOldMatIds_GLOBAL(RECL_FizMatCounter_GLOBAL+i)=RECL_FIZolds(i)
        RECL_FizNewMatIds_GLOBAL(RECL_FizMatCounter_GLOBAL+i)=RECL_FizNewPre(i)
      end do
      !if (isTEST >= 1) write(333, *) '  RECL_FizOldMatIds_GLOBAL=',RECL_FizOldMatIds_GLOBAL(1:RECL_FizMatCounter_GLOBAL)
      !if (isTEST >= 1) write(333, *) '  RECL_FizNewMatIds_GLOBAL=',RECL_FizNewMatIds_GLOBAL(1:RECL_FizMatCounter_GLOBAL)
      RECL_FizMatCounter_GLOBAL = RECL_FizMatCounter_GLOBAL + AddMatsPreCount
      !if (isTEST >= 1) write(333, *) '  RECL_FizMatCounter_GLOBAL=',RECL_FizMatCounter_GLOBAL
    end if


    !СТАВИМ СТАТУС ПОСЛЕДНИХ ЗОН НА 3-КУ - конструкционные материалы
    do i=1,M1+AddMatsUniqueCount   + AddMatsPreUniqueCount
      if (i > Nf + AddMatsUniqueCount   + AddMatsPreUniqueCount) then
        RECL_nzon_type(i) = 3   !конструкционный материал
      end if
    end do

    !ИЗМЕНЯЕМ В KT ВСЕ НЕДЕЛЯЩИЕСЯ СОСТАВЫ, ДОБАВЛЯЯ К КАЖДОМУ НОМЕРУ КОЛ-ВО НОВЫХ СОСТАВОВ AddMatsUniqueCount
    write(333, *) 'Adding numbers for constructive materials (KM) fiz zones...'
    do i=1,maxNk
      do j=1,MaxNSLZ
        if (KTx2(i,j) > Nf) then
          KTx2(i,j) = KTx2(i,j) + AddMatsUniqueCount + AddMatsPreUniqueCount
        end if
      end do
    end do



    !КОПИРУЕМ ГОД ЗАГРУЗКИ КМ В КОНЕЦ МАССИВА
    !RECL_FizStepLoaded = RECL_FizStepLoaded(1:M1+AddMatsUniqueCount+AddMatsPreUniqueCount)
    do i=Nf+1,M1
      newIndex = i + AddMatsUniqueCount+AddMatsPreUniqueCount
      RECL_FizStepLoaded(newIndex) = RECL_FizStepLoaded(i)
    end do
    !СТАВИМ НОВЫЕ ГОДА ЗАГРУЗКИ ДЛЯ НОВЫХ МАТ СОСТАВОВ
    do i=1,M1+AddMatsUniqueCount+AddMatsPreUniqueCount
      do j=1,AddMatsCount
        if (RECL_FizNewIds(j) == i) then
          RECL_FizStepLoaded(i) = StepGlobal
        end if
      end do
      do j=1,AddMatsPreCount
        if (RECL_FizNewPre(j) == i) then
          RECL_FizStepLoaded(i) = StepGlobal
        end if
      end do
    end do


    !ОТОБРАЖАЕМ СОДЕРЖИМОЕ МАССИВА KT В УДОБНОМ ВИДЕ
    write(333, *) '----'
    write(333, *) 'KT - ZONES ADDED KM MATERIALS'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk
      write(333, trim(FMT2)) KTx2(i,1:MaxNSLZ)
    enddo



    !КОПИРУЕМ В КОНЕЦ KT ТИПЫ ТВСОК, ЧТОБЫ ПОТОМ ОТРЕДАКТИРОВАТЬ СОСТАВЫ ПО ВЫСОТЕ
    write(333, *) 'Coping fiz zones in KT of the recycled TVS to the end...'
    allocate(KTx2_TEMP(1:maxNk,1:MaxNSLZ))
    KTx2_TEMP = KTx2
    deallocate(KTx2)
    allocate(KTx2(1:(maxNk+AddNewTVSUniqueType), 1:MaxNSLZ))
    do i=1,maxNk
      do j=1,MaxNSLZ
        KTx2(i,j)=KTx2_TEMP(i,j)
      end do
    end do
    deallocate(KTx2_TEMP)
   ! KTx2=KTx2(1:(maxNk+AddNewTVSUniqueType), 1:MaxNSLZ)
    newIndex = -1
    do k=1,AddNewTVSType
      do i=1,maxNk
        !проверяем только те строки, которые нам нужны (= номеру ТВСки)
        if (i == RECL_TVSnumToReload(k)) then
          do j=1,MaxNSLZ
            newIndex = RECL_TVSnumCaption(k)  !соответствие между старыми и новыми номерами ТВС
            KTx2(newIndex,j) = KTx2(i,j)
          end do
        end if
      end do
    end do
    if (isTEST >= 2) write(333, *) 'KTx2_EXTENDED= ',KTx2


    !ОТОБРАЖАЕМ СОДЕРЖИМОЕ МАССИВА KT В УДОБНОМ ВИДЕ
 !   write(333, *) '----'
 !   write(333, *) 'KT - ZONES EXTENDED'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk+AddNewTVSUniqueType
!      write(333, trim(FMT2)) KTx2(i,1:MaxNSLZ)
    enddo


    !ТЕПЕРЬ, ЧТО НУЖНО СДЕЛАТЬ В KT-ЭТО ЗАМЕНИТЬ НОМЕРА НОВЫХ ДЕЛЯЩИХСЯ СОСТАВОВ НА НОВЫЕ НОМЕРА СОСТАВОВ, КОТОРЫЕ ПОТОМ МЫ БУДЕМ ОБРАБАТЫВАТЬ
    write(333, *) '----'
    write(333, *) 'Change numbers of material in the KT map to new materials numbers'
    do i=1,(maxNk+AddNewTVSUniqueType)
      !нужно заменить материалы только в перегружаемых ТВСках, поэтому сначала проверяем, относится ли данная ТВСка к перегружаемым?
      isInArr = .false. !сначала считаем, что не относится
      do j=1,AddNewTVSType
        if (RECL_TVSnumCaption(j)==i) then !да, этот номер перегружается
          isInArr = .true.
        end if
      end do
      if (isInArr) then
        do j=1,MaxNSLZ
          do k=1,AddMatsCount
            if (KTx2(i,j) == RECL_FIZids(k)) then
              !if (isTEST >= 2) write(333, *) 'RECL_FIZids(',k,')= ',RECL_FIZids(k);
              !if (isTEST >= 2) write(333, *) 'RECL_FizNewIds(',k,')= ',RECL_FizNewIds(k);
              KTx2(i,j) = RECL_FizNewIds(k)
            end if
          end do
          !if (isTEST >= 2) write(333, *) 'KTx2(',i,',',j,')= ',KTx2(i,j);
        end do
      end if
    end do


    if (StepGlobal > 1) then
      !ЗАМЕНЯЕМ НОМЕРА СОСТАВОВ В ПЕРЕГРУЖЕННЫХ ТВС НА НОВЫЕ НОМЕРА СОСТАВОВ
      write(333, *) '----'
      write(333, *) 'Change numbers of material of previoulse unloaded zones in the KT map to new materials numbers'
      do i=1,maxNk
        !нужно заменить материалы только в перегружаемых ТВСках, поэтому сначала проверяем, относится ли данная ТВСка к перегружаемым?
        isInArr = .false. !сначала считаем, что не относится
        do j=1,MinusOneTVSCount !для того количества добавленных на предыдущем шаге ТВСок
          if (RECL_TVSnumUnloaded(j)==i) then !ищем номер каждой перегруженной ТВСки... и да, если это он
            isInArr = .true.
          end if
        end do
        if (isInArr) then
          do j=1,MaxNSLZ
            do k=1,AddMatsPreCount
              if (KTx2(i,j) == RECL_FIZolds(k)) then
                KTx2(i,j) = RECL_FizNewPre(k)
              end if
            end do
           ! if (isTEST >= 2) write(333, *) 'KTx2(',i,',',j,')= ',KTx2(i,j);
          end do
        end if
      end do
    end if
    do i=1,(maxNk+AddNewTVSUniqueType)
      if (isTEST >= 2) write(333, *) 'RECL_TVSnum_Enabled(',i,')=',RECL_TVSnum_Enabled(i)
    end do
    !ОБРАБОТКА МАССИВОВ KT И NK ЗАВЕРШЕНА
    !===============================




    !===============================
    !ОБРАБОТКА МАССИВА СОСТАВОВ AD
    !ДЕЛАЕМ ДВУМЕРНЫЙ МАССИВ ИЗ AD, ЧТОБЫ ЛЕГЧЕ БЫЛО С НИМ ОБРАЩАТЬСЯ
    k = 1 !увеличиваем номера для неделящихся материалов
    !allocate(ADx2(1:M1, 1:Ner1))
    allocate(ADx2(1:(M1+AddMatsUniqueCount    + AddMatsPreUniqueCount), 1:Ner1))
    do i=1,M1
      do j=1,Ner1
        ADx2(i,j) = Ad(k) !заполняем двухмерный массив
        k = k + 1
      end do
    end do
    !КОПИРУЕМ ИЗОТОПНЫЕ КМ В КОНЕЦ
    newIndex = -1
    !ADx2=ADx2(1:(M1+AddMatsUniqueCount    + AddMatsPreUniqueCount), 1:Ner1)
    do i=Nf+1,M1
      do j=1,Ner1
        newIndex = i + AddMatsUniqueCount      + AddMatsPreUniqueCount
        ADx2(newIndex,j) = ADx2(i,j) !заполняем двухмерный массив
        !if (isTEST >= 2) write(333, *) 'ADx2_OLD(',i,',',j,')= ',ADx2(i,j)
        !if (isTEST >= 2) write(333, *) 'ADx2_NEW(',newIndex,',',j,')= ',ADx2(newIndex,j)
      end do
    end do
    !КОПИРУЕМ СОСТАВЫ ДЕЛЯЩИХСЯ ИЗОТОПОВ В НОВЫЕ МЕСТА, СОЗДАВАЯ НОВЫЕ СОСТАВЫ
    n = 1
    newIndex = -1
    if (isTEST >= 2) write(333, *) '----------'
    if (isTEST >= 2) write(333, *) 'Added Global recycling zones'
    if (isTEST >= 2) write(333, *) 'Compared to: ',RECL_FIZids
    do k=1,AddMatsCount
      do i=1,M1+AddMatsUniqueCount    + AddMatsPreUniqueCount
        !if (isTEST >= 2) write(333, *) 'i=',i
        !if (isTEST >= 2) write(333, *) 'i=',i
        if (i == RECL_FIZids(k)) then
          do j=1,Ner1
            newIndex = RECL_FizNewIds(n)
            ADx2(newIndex,j) = ADx2(i,j) !заполняем двухмерный массив
            !if (isTEST >= 2) write(333, *) 'ADx2_OLD(',i,',',j,')= ',ADx2(i,j);
            !if (isTEST >= 2) write(333, *) 'ADx2_NEW(',newIndex,',',j,')= ',ADx2(newIndex,j);
          end do
          !if (isTEST >= 2) write(333, '(a,i4,a,i4)') '  Copy concentrations of recycled Zone ',RECL_FIZids(n),' => ',RECL_FizNewIds(n)
          !Сохраняем в глобальные переменные, чтобы потом можно было воспользоваться при перегрузках
          RECL_FizMatCounter_GLOBAL_R = RECL_FizMatCounter_GLOBAL_R + 1
          !if (isTEST >= 2) write(333, *) '  RECL_FizMatCounter_GLOBAL_R=',RECL_FizMatCounter_GLOBAL_R
          !RECL_FizOldMatIds_GLOBAL_R=RECL_FizOldMatIds_GLOBAL_R(1:RECL_FizMatCounter_GLOBAL_R)
          RECL_FizOldMatIds_GLOBAL_R(RECL_FizMatCounter_GLOBAL_R)=RECL_FIZids(n)
          !if (isTEST >= 2) write(333, *) '  RECL_FizOldMatIds_GLOBAL_R=',RECL_FizOldMatIds_GLOBAL_R(1:RECL_FizMatCounter_GLOBAL_R)
          !RECL_FizNewMatIds_GLOBAL_R=RECL_FizNewMatIds_GLOBAL_R(1:RECL_FizMatCounter_GLOBAL_R)
          RECL_FizNewMatIds_GLOBAL_R(RECL_FizMatCounter_GLOBAL_R)=RECL_FizNewIds(n)
          !if (isTEST >= 2) write(333, *) '  RECL_FizNewMatIds_GLOBAL_R=',RECL_FizNewMatIds_GLOBAL_R(1:RECL_FizMatCounter_GLOBAL_R)
          n = n + 1
        end if
      end do
    end do



    !ЗАПОЛНЯЕМ МАССИВ НОВЫХ ЛИБО ПЕРЕРАБОТАННЫХ КОНЦЕНТРАЦИЙ
    if (StepGlobal > 1) then
      !Исходные концентрации начальных ТВС будем искать на самом первом шаге в исходных данных
      if (StepGlobal.LE.(nVRH+nRecl+1)) then
        AddedStr = AddEndOfOutputFile(1) !СНАЧАЛА нас интересует время на начальный момент (1-й - 1load)
        curFile_CONC = 'recycle\1stp\1_conc'//trim(AddedStr)//'.txt' !концентрации
        if (isTEST >= 2) write(333, *) 'curFile_CONC = '//trim(curFile_CONC )
        open (55,file=trim(curFile_CONC),status='old')
      end if
      n = 1
      newIndex = -1
      do m=1,AddMatsPreCount
        newIndex = RECL_FizNewPre(m)
        if (isTEST >= 2) write(333, *) 'ZONE ',newIndex,' IS PLACE TO SET NEW CONC FOUND!'
        if (StepGlobal > (nVRH+nRecl+1)) then
          !если уже можно загрузить переработку
          if (isTEST >= 2) write(333, *) 'Loading recycling TVS into zone ',newIndex
          do j=1,Ner1 !для каждого изотопа читаем его данные
            ADx2(newIndex,j) = IsotReclConc(j) !посчитанные концентрации изотопов после переработки
          end do
          if (isTEST >= 2) write(333, *) 'Loading recycling TVS into zone ',newIndex,'... DONE!'
        else
          !ИСХОДНЫЕ КОНЦЕНТРАЦИИ ЗАГРУЖАЮТСЯ УСПЕШНО - ВСЁ ОКЕЙ
          !если ещё время не пришло для загрузки переработки, то нужно добавить исходные составы, которые можно найти в исходном файле с концентрациями
          if (isTEST >= 2) write(333, *) 'Loading initial TVS'
          do j=1,RECL_FizMatCounter_GLOBAL
            if (newIndex == RECL_FizNewMatIds_GLOBAL(j)) then
              if (StepGlobal == 2) then
                NumZonFound = RECL_FizOldMatIds_GLOBAL(j)  !на первых шагах лучше использовать новые номера составов, старые номера могли быть стерты из файла за ненадобностью
              else
                !ищем исходный номер среди массива номеров, перегружаемых для переработки зон
                do k=1,RECL_FizMatCounter_GLOBAL_R
                  if (RECL_FizNewMatIds_GLOBAL_R(k) == RECL_FizOldMatIds_GLOBAL(j)) then
                    NumZonFound = RECL_FizOldMatIds_GLOBAL_R(k)  !находим, какая зона соответствовала данной
                  end if
                end do
              end if
            end if
          end do
          !ищем исходные концентрации для этой зоны
          ios = 0
          !iostat:
            ! = -1 error: end of file
            ! = -2 error: end of record
          !узнаем кол-во строк в файле
          if (isTEST >= 2) write(333, *) '-----------Finding zone ',NumZonFound
          do while (ios /= -1)
            read(55, '(A)', advance = "yes", iostat = ios) Str250
            if (ios /= -1) then
              if (Str250(1:11).eq.'       ЗОНА') then
                read(Str250,'(a11,I5)') Str11, NumZonIn !      ЗОНА    1.
                read(55,*) ! TYPE:            1
                read(55,*) ! Loaded on Step 1
                if (NumZonIn == NumZonFound) then
                  if (isTEST >= 2) write(333, *) 'FOUND!'
                  do j=1,Ner1 !для каждого изотопа читаем его данные
                    read (55,'(i3.3, e14.5)') IsotID, ConcIn
                    ADx2(newIndex,j) = ConcIn !Если 1-й или 3-й тип, то оставляем тем же составом, который был на начало шага начале
                    !if (isTEST >= 2) write(333, *) 'ADx2_OLD(',i,',',j,')= ',ADx2(i,j);
                    !if (isTEST >= 2) write(333, *) 'ADx2_NEW(',newIndex,',',j,')= ',ADx2(newIndex,j);
                  end do
                end if
              end if
            end if
          end do
          rewind(55)
        end if
      end do
      if (StepGlobal.LE.(nVRH+nRecl+1)) close(55)
    end if
    deallocate(IsotReclConc)  !больше на данном шаге нам переработанные концентрации не понадобятся
    !if (isTEST >= 2) write(333, *) 'ADx2= ',ADx2;


    !ПОСЛЕДНЕЕ - СОХРАНЯЕМ ДВУХМЕРНЫЙ МАССИВ В ОДНОМЕРНЫЙ
    deallocate(KT)
    allocate(KT(1:((maxNk+AddNewTVSUniqueType)*MaxNSLZ)))
    !KT = KT(1:((maxNk+AddNewTVSUniqueType)*MaxNSLZ))
    n = 1
    !if (isTEST >= 2) write(333, *) '--------------'
   ! if (isTEST >= 2) write(333, *) 'KT'
    do i=1,(maxNk+AddNewTVSUniqueType)
     ! if (isTEST >= 2) write(333, *) 'i=',i
      do j=1,MaxNSLZ
        KT(n) = KTx2(i,j)
       ! if (isTEST >= 2) write(333, *) '   KTx2(',i,',',j,');= ',KTx2(i,j)
        n = n + 1
      end do
    end do


    !ОТОБРАЖАЕМ СОДЕРЖИМОЕ МАССИВА KT В УДОБНОМ ВИДЕ
    write(333, *) '----'
    write(333, *) 'KT - ZONES END MAP'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk+AddNewTVSUniqueType
      write(333, trim(FMT2)) KT((i-1)*MaxNSLZ+1:i*MaxNSLZ)
    enddo


    !ПРОВЕРЯЕМ ПРИСУТСТВИЕ ВСЕХ ПОРЯДКОВЫХ НОМЕРОВ В КАРТЕ ЗОН (В МАССИВЕ KT), чтобы исключить если что какие-то из отображения в результатах
    if (isTEST >= 2) write(333, *) '     CHECK NOT USED MATERIAL ZONES IN KT ARRAY'
    n = 0 !количество неиспользуемых материалов
    do i=1,(M1+AddMatsUniqueCount     + AddMatsPreUniqueCount)
      isInArr = .false. !по умолчанию считаем, что её в массиве нет
!      CheckCurFiz: do j=1,(maxNk+AddNewTVSUniqueType)
      CheckCurFiz: do j=1,((maxNk+AddNewTVSUniqueType)*MaxNSLZ)
!        do k=1,MaxNSLZ
!          if (KTx2(j,k) == i) then
           if (KT(j) == i) then
            isInArr = .true.  !хотя бы один номер найден в массиве, значит оставляем эту зону - ОК
            exit CheckCurFiz
          end if
!        end do
      end do  CheckCurFiz
      !Исключаем данную зону из рассмотрения, если её нет вообще в массиве
      if (.not.isInArr) then
        do k=1,Ner1
          ADx2(i,k) = 1E-10
          !if (isTEST >= 2) write(333, *) '        ADx2(',i,',',k,')=',ADx2(i,k)
        end do
        !исключаем зону из расчёта функционалов, освобождая её для будущего использования
        if (RECL_nzon_type(i).NE.0) then
          RECL_nzon_type(i)=5
          n = n + 1
        end if
        if (isTEST >= 1) write(333, *) 'Zone ',i,' has been excluded from the result file! //'
      end if
    end do
    !ПРОВЕРЯЕМ ПРИСУТСТВИЕ ВСЕХ ФИЗИЧЕСКИХ ЗОН В КАРТЕ АКТИВНОЙ ЗОНЫ (В СТРОКЕ МАССИВА KT, ПОЛУЧЕННОМ ИЗ МАССИВА NK), чтобы исключить если что какие-то из отображения в результатах
    n = 0
    allocate(FizZoneStatus_TEMP(1:M1+AddMatsUniqueCount    + AddMatsPreUniqueCount))
    FizZoneStatus_TEMP = 0 !по умолчанию считаем, что зон нет в активной зоне
    !проверяем для каждой ТВС в текущей картограмме
    !write(333, *) '   Finding Nks'
    do i=1,NkDimention
      m = Nk(i)  !текущий проверяемый номер ТВСки
      !write(333, *) 'Nk(i)=',Nk(i)
      do j=1,MaxNSLZ !проверяем все номера материалов в ТВСке
        k = KTx2(m,j)
        FizZoneStatus_TEMP(k) = 1  !этот номер материала был найден - окей
        !write(333, *) 'FizZoneStatus_TEMP(',k,')=',FizZoneStatus_TEMP(k)
      end do
    end do
    do i=1,M1+AddMatsUniqueCount    + AddMatsPreUniqueCount
      !если какой-то материал отсутствует в картограмме, то выключаем его и убираем концентрации
      if (FizZoneStatus_TEMP(i) == 0) then
        do k=1,Ner1
          ADx2(i,k) = 1E-10
        end do
        !исключаем зону из расчёта функционалов, освобождая её для будущего использования
        if (RECL_nzon_type(i).NE.0) then
          RECL_nzon_type(i)=5
!          RECL_FizNewIds())
          n = n + 1
        end if
        if (isTEST >= 1) write(333, *) 'Zone ',i,' has been excluded from the result file! \\'
      end if
    end do
    deallocate(FizZoneStatus_TEMP)


    !Уменьшаем в KT номера на количество свободных номеров составов
    !AddMatsUniqueCount = AddMatsUniqueCount - n
    write(333, *) 'Using free materials'
    do i=1,(M1+AddMatsUniqueCount     + AddMatsPreUniqueCount)
      if (RECL_nzon_type(i) == 5) then
        do j=1,maxNk+AddNewTVSUniqueType
          do k=1,MaxNSLZ
            if (KTx2(j,k) > i) then
              !KTx2(j,k) = KTx2(j,k) - 1
            end if
          end do
        end do
      end if
    end do


    !Отображаем соответствие между старыми и новыми номерами
    do i=1,AddMatsCount
      FindNewMatIds: do j=1,maxNk
        do k=1,MaxNSLZ
          if (KTx2_ORIGINAL(j,k) == RECL_FIZids(i)) then
            !RECL_FizNewIds(i) = KTx2(j,k)
           !write(333, *) 'KTx2_ORIGINAL(',j,',',k,')=',KTx2_ORIGINAL(j,k)
            write(333, *) '   KTx2(',j,',',k,')=',KTx2(j,k)
            exit FindNewMatIds
          end if
        end do
      end do FindNewMatIds
    end do
    write(333, *) 'New captions of new materials for recycling (RECL_FizNewIds): ',RECL_FizNewIds
    write(333, *) 'New UNIQUE material count for recycling (AddMatsUniqueCount): ',AddMatsUniqueCount

    deallocate(Ad)
    allocate(Ad(1:((M1+AddMatsUniqueCount       +AddMatsPreUniqueCount)*Ner1)))
    n = 1
    do i=1,(M1+AddMatsUniqueCount    + AddMatsPreUniqueCount)
      do j=1,Ner1
        Ad(n) = ADx2(i,j)
        n = n + 1
      end do
    end do
    !ОБРАБОТКА МАССИВА СОСТАВОВ AD ЗАВЕРШЕНА
    !===============================


    !===============================
    !НАЗНАЧАЕМ ТЕМПЕРАТУРУ ДЛЯ МАТ. СОСТАВА В СООТВЕТСТВИЕ С ЕГО НОМЕРОМ
    allocate(Tem_Copy(1:M1))
    Tem_Copy = Tem
    deallocate(Tem)
    allocate(Tem(1:M1+AddMatsUniqueCount   + AddMatsPreUniqueCount))
    do i=1,M1
      Tem(i) = Tem_Copy(i)
    end do
    deallocate(Tem_Copy)

    !ПЕРЕНОСИМ В КОНЕЦ ТЕМПЕРАТУРЫ ДЛЯ КМ
    do i=1,(M1+AddMatsUniqueCount   + AddMatsPreUniqueCount)
      if ((i > Nf).AND.(i<=M1)) then
        newIndex = i + AddMatsUniqueCount    + AddMatsPreUniqueCount
        Tem(newIndex) = Tem(i)
      end if
    end do
    !ПЕРЕНОСИМ ТЕМПЕРАТУРЫ СКОПИРОВАННЫХ СОСТАВОВ
    do m=1,AddMatsCount
      do i=1,(M1+AddMatsUniqueCount + AddMatsPreUniqueCount)
        if (i == RECL_FIZids(m)) then
          newIndex = RECL_FizNewIds(m)
          Tem(newIndex) = Tem(i)
        end if
      end do
    end do
    if (StepGlobal > 1) then
      do m=1,AddMatsPreCount
        do i=1,(M1+AddMatsUniqueCount     + AddMatsPreUniqueCount)
          if (i == RECL_FIZolds(m)) then
            newIndex = RECL_FizNewPre(m)
            Tem(newIndex) = Tem(i)
          end if
        end do
      end do
    end if
    !НАЗНАЧАЕМ НОВУЮ ТЕМПЕРАТУРУ - КОНЕЦ
    !===============================


    !===============================
    !ПЕРЕСЧИТЫВАЕМ ВСЕ ОСТАЛЬНЫЕ ПРОСТЫЕ ЧИСЛА
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Count of assebmlies to by recycled on this step (AddNewTVSType)= ',AddNewTVSType
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Count of new numbers of assebmlies in the Core (AddNewTVSUniqueType)= ',AddNewTVSUniqueType
    !То количество материалов (физических зон), которое мы добавили для учета перегрузки на следующем шаге
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Materials quantity on current step to be recycled (AddMatsCount) = ',AddMatsCount
    !То количество материалов (физических зон), которое мы добавили для замены перегруженных зон на предыдущем шаге
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Materials quantity for unloaded materials (AddMatsPreCount) = ',AddMatsPreCount
    RECL_ERR_FizZone2 = AddMatsCount !запоминаем то кол-во материалов, которое мы выгрузили
    if (StepGlobal == 1) RECL_ERR_FizZone1 = RECL_ERR_FizZone2 !в начальный момент времени ничего не проверяем
    !То количество новых материалов (физических зон), которое мы должны добавить (уникальных новых материалов)
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Added new fiz zones for recycling (AddMatsUniqueCount) = ',AddMatsUniqueCount
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Added new fiz zones for previously unloaded &
    & materials (AddMatsPreUniqueCount) = ',AddMatsPreUniqueCount
    NewMatsCount = AddMatsUniqueCount + M1    + AddMatsPreUniqueCount
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'New value of materials quantity (AddMatsUniqueCount &
    &+ M1 + AddMatsPreUniqueCount) = ',NewMatsCount
    !старое количество делящихся материалов
    OldMatsCount = Nf
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 2) write(333, *) 'Old value of fissile materials quantity = ',OldMatsCount
    !Добавляем это кол-во материалов, где это требуется
    M1 = M1 + AddMatsUniqueCount    + AddMatsPreUniqueCount
    M1v = M1v + AddMatsUniqueCount     + AddMatsPreUniqueCount
    Naz = Naz + AddMatsUniqueCount   + AddMatsPreUniqueCount
    Nt = Nt + AddMatsUniqueCount   + AddMatsPreUniqueCount
    Nf = Nf + AddMatsUniqueCount   + AddMatsPreUniqueCount
    if (Ntor /= 0) Ntor = Ntor + AddMatsUniqueCount   + AddMatsPreUniqueCount
    if (Nbok /= 0) Nbok = Nbok + AddMatsUniqueCount   + AddMatsPreUniqueCount
    !Формируем массив номеров новых зон
    deallocate(Nzon)
    deallocate(Jpfz)
    allocate(Nzon(1:M1))
    allocate(Jpfz(1:NT))
    allocate(Ntnz_temp(1:NT-AddMatsUniqueCount   - AddMatsPreUniqueCount))
    Ntnz_temp = Ntnz
    deallocate(Ntnz)
    allocate(Ntnz(1:NT))
    do i=1,NT-AddMatsUniqueCount   - AddMatsPreUniqueCount
      Ntnz(i)=Ntnz_temp(i)
    end do
    deallocate(Ntnz_temp)
    i = Nf+1 !смещаем массив для НЕделящихся материалов
    do while (i > Nt)
      Ntnz(i)=Ntnz(i-AddMatsUniqueCount   - AddMatsPreUniqueCount)
      i = i + 1
    end do
    i = 1
    Ntnz = 4 !по умолчанию делаем все четверки (4 - это зона, относящаяся к реактору вообще)
    do while (i <= M1)
      Nzon(i)=i
      if (i <= Nf) then
        Ntnz(i)=1
      end if
      if (RECL_nzon_type(i) == 0) then
        Jpfz(i)=0
        Ntnz(i)=0
      else if (RECL_nzon_type(i) == 5) then
        Jpfz(i)=0
        Ntnz(i)=0
      else
        Jpfz(i)=i
      end if
      i = i + 1
    enddo
    !Формируем массив принадлежности к разным зонам
    i = OldMatsCount + 1 !а теперь смещаем массив для делящихся зон
    j = 1
    ! if (isTEST >= 2) write(333, *) 'Ntnz= ',Ntnz;
    !ПЕРЕСЧЁТ РАЗНЫХ ПРОСТЫХ ЧИСЕЛ НОВОГО ВХОДНОГО ФАЙЛА ЗАВЕРШЁН
    !===============================


    write(333,*) '----'
    write(333,*) 'Zone type (0 - disabled, 1 - usual, 2 - recycled at the end of the step, 3 - km, 5 - free)'
    do i=1,M1
       write(333,*) '  RECL_nzon_type(',i,')=',RECL_nzon_type(i)
    end do
    write(333,*) '----'
    write(333,*) 'Years of zones loaded to the core'
    do i=1,M1
       write(333,*) '  RECL_FizStepLoaded(',i,')=',RECL_FizStepLoaded(i)
    end do
    write(333,*) '----'
    write(333,*) 'History of the changes of fiz zones numbers for recycling on this step'
    do i=1,RECL_FizMatCounter_GLOBAL_R
      write(333,*) '  ',RECL_FizOldMatIds_GLOBAL_R(i),'=>',RECL_FizNewMatIds_GLOBAL_R(i)
    end do
    write(333,*) '----'
    write(333,*) 'History of the changes of fiz zones numbers for loading new TVS'
    do i=1,RECL_FizMatCounter_GLOBAL
      write(333,*) '  ',RECL_FizOldMatIds_GLOBAL(i),'=>',RECL_FizNewMatIds_GLOBAL(i)
    end do
    write(unt1, NML = Mim)
    write(unt1, NML = D26)
    write(unt1, NML = Dan)
    write(unt1, NML = Obr)
    write(unt1, NML = Upbn)
    close(unt1)

    deallocate(RECL_FizNewIds)
    deallocate(RECL_FIZids)
    deallocate(RECL_FIZids_TEMP)
    deallocate(RECL_TVSnumToReload)
    deallocate(ADx2)
    deallocate(KTx2)
    deallocate(KTx2_ORIGINAL)
    deallocate(RECL_TVSnumCaption)
    if (StepGlobal > 1) then
      deallocate(RECL_FIZolds)
      deallocate(RECL_FIZolds_TEMP)
      deallocate(RECL_FizNewPre)
      deallocate(RECL_TVSnumUnloaded)
    end if
end subroutine
