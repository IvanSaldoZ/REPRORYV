!„»“ј≈ћ »—’ќƒЌџ≈ ƒјЌЌџ≈ JARFR (NAMELISTы MIM, D26, DAN и т.д. придетс€ воспроизвести) » REPRORYV
subroutine ReadJARFRInputData(input_file_JARFR)
    ! переменные из JARFR описаны в этом модуле
    use CommonModJARFR
    character*60 :: input_file_JARFR

    if (StepGlobal > 1) then
      deallocate(Sostav)
      deallocate(nzon)
      deallocate(Ad)
      deallocate(Tem)
      deallocate(ncol)
      !&D26
      deallocate(Nslz)
      deallocate(KT)
      deallocate(Ns)
      deallocate(Nk)
      deallocate(Nsm)
      deallocate(Hzr)
      !&DAN
      deallocate(jprw)
      !&Obr
      deallocate(Ngor)
      deallocate(Nvos)
      deallocate(Ntnz)
      deallocate(Jpfz)
      !&Upbn
      deallocate(Jprn)
      deallocate(Jdpt)
      deallocate(T_Usr)
      deallocate(PWM)
    end if


    !выдел€ем пам€ть дл€ того, чтобы прочитать список
    !&MIM
    allocate(Sostav(1:99999))
    allocate(nzon(1:99999))
    allocate(Tem(1:99999))
    allocate(ad(1:9999999))
    allocate(ncol(1:26))
    !&D26
    allocate(Nslz(1:99999))
    allocate(KT(1:99999))
    allocate(Ns(1:99999))
    allocate(Nk(1:99999))
    allocate(Nsm(1:99999))
    allocate(Hzr(1:99999))
    !&DAN
    allocate(jprw(1:99999))
    !&Obr
    allocate(Ngor(1:99999))
    allocate(Nvos(1:99999))
    allocate(Ntnz(1:99999))
    allocate(Jpfz(1:99999))
    !&Upbn
    allocate(Jprn(1:99999))
    allocate(Jdpt(1:99999))
    allocate(T_Usr(1:99999))
    allocate(PWM(1:99999))

    !получаем кол-во элементов в массиве
    open(unt1, file = input_file_JARFR, status = 'UNKNOWN')
    READ(unt1, NML = Mim)
    READ(unt1, NML = D26)
    READ(unt1, NML = Dan)
    READ(unt1, NML = Obr)
    READ(unt1, NML = Upbn)
    close(unt1)

    !ищем размерность массива картограммы Nk
    i = 1
    NkDimention = 0
    do while (i <= Nr1)
      NkDimention = NkDimention + Ns(i)
      i = i + 1
    enddo
    !ищем максимальное значение в картограмме Nk - это размерность массива KT
    i = 1
    maxNk = 0
    do while (i <= NkDimention)
      if (Nk(i) > maxNk) maxNk = Nk(i)
      i = i + 1
    enddo
    !ищем максимальное значение по сло€м в массиве NSLZ  - это также размерность массива KT
    !устанавливаем размерность массива распределени€ физ-зон по высоте как 16x19 (кол-во типов “¬— в картограмме MaxNk умножить на кол-во уникальных слоев по высоте MaxNSLZ)
    i = 1
    MaxNSLZ = 0
    do while (i <= Nr2)
      if (Nslz(i) > MaxNSLZ) MaxNSLZ = Nslz(i)
      i = i + 1
    enddo


    !call DE_ALLOCATE_MEMEORY_OF_ARRAYS
    deallocate(Sostav)
    deallocate(nzon)
    deallocate(Ad)
    deallocate(Tem)
    deallocate(ncol)
    !&D26
    deallocate(Nslz)
    deallocate(KT)
    deallocate(Ns)
    deallocate(Nk)
    deallocate(Nsm)
    deallocate(Hzr)
    !&DAN
    deallocate(jprw)
    !&Obr
    deallocate(Ngor)
    deallocate(Nvos)
    deallocate(Ntnz)
    deallocate(Jpfz)
    !&Upbn
    deallocate(Jprn)
    deallocate(Jdpt)
    deallocate(T_Usr)
    deallocate(PWM)

    !но теперь у нас есть точное количество размерностей массивов

    !¬џƒ≈Ћя≈ћ ѕјћя“№ ƒЋя ћј——»¬ќ¬
    !----------------------------------
    !call ALLOCATE_MEMEORY_FOR_ARRAYS
    !----------------------------------




    !open(unt1, file = inp_temp, status = 'UNKNOWN')
    open(unt1, file = input_file_JARFR, status = 'UNKNOWN')

    !выдел€ем пам€ть дл€ того, чтобы прочитать список
    !&MIM
    allocate(Tem(1:M1))
    allocate(Sostav(1:Ner1))
    allocate(Nzon(1:M1))
    allocate(Ad(1:M1*Ner1))
    allocate(Ncol(1:26))
    tem=-1
    sostav=-1
    nzon=-1
    ad=-1
    ncol=-1
    READ(unt1, NML = Mim)
    !tem=tem(1:M1)
    !sostav=sostav(1:Ner1)
    !nzon=nzon(1:M1)
    !ad=ad(1:(M1*Ner1))

    !&D26
    allocate(Nslz(1:Nr2))
    allocate(Hzr(1:Nr2))
    allocate(Ns(1:Nr1))
    allocate(NSM(1:6))
    allocate(Nk(1:NkDimention))
    allocate(KT(1:(maxNk*MaxNSLZ)))
    Nslz = -1
    Hzr = -1
    Ns = -1
    Nsm = -1
    Nk = -1
    KT = -1
    !Nslz=Nslz(1:Nr2)
    !Hzr=Hzr(1:Nr2)
    !Ns=Ns(1:Nr1)
    !NSM=NSM(1:6)
    !Nk=Nk(1:NkDimention)
    !KT=KT(1:(maxNk*MaxNSLZ))
    READ(unt1, NML = D26)

    !&DAN
    allocate(jprw(1:NR2))
    Jprw = -1
    READ(unt1, NML = Dan)
    !jprw=jprw(1:NR2)

    !&Obr
    allocate(Ngor(1:Ngel))
    allocate(Nvos(1:Nvel))
    allocate(Ntnz(1:Nt))
    allocate(Jpfz(1:Nt))
    Ngor=-1
    Nvos=-1
    Ntnz=-1
    Jpfz=-1
    !Ngor=Ngor(1:Ngel)
    !Nvos=Nvos(1:Nvel)
    !Ntnz=Ntnz(1:Nt)
    !Jpfz=Jpfz(1:Nt)
    READ(unt1, NML = Obr)

    !&Upbn
    !Jprn=Jprn(1:Ndt)
    !Jdpt=Jdpt(1:Ndt)
    !Jprw=Jprw(1:Nr2)
    !T_Usr=T_Usr(1:Ndt)
    !PWM=PWM(1:Ndt)
    allocate(Jprn(1:Ndt))
    allocate(Jdpt(1:Ndt))
    allocate(T_Usr(1:Ndt))
    allocate(PWM(1:Ndt))
    Jprn=-1
    Jdpt=-1
    Jprw=-1
    T_Usr=-1
    PWM=-1
    READ(unt1, NML = Upbn)
    close(unt1) ! «акрываем исходный файл JARFR

    !путь до бат-файла JARFR
    open(unt1, file = 'settings\jarfrbatfile.txt', status = 'OLD')
    read(unt1,'(A)') JARFRbatfilename
    close(unt1)

    !—оставл€ем новый входной файл JARFR
    open(unt4, file = inp_new, status = 'UNKNOWN')
    write(unt4, NML = Mim)
    write(unt4, NML = D26)
    write(unt4, NML = Dan)
    write(unt4, NML = Obr)
    write(unt4, NML = Upbn)
    close(unt4)


    !„итаем входной файл программы REPRORYV
    if (StepGlobal == 1) then
      !ƒќѕќЋЌ»“≈Ћ№Ќќ ¬џƒ≈Ћя≈ћ ѕјћя“№ ƒЋя ѕ≈–≈ћ≈ЌЌџ’, Ќ≈ќЅ’ќƒ»ћџ’ ƒЋя ѕ≈–≈√–”« »
      allocate(RECL_nzon_type(1:M1)) !пометка того, нужно ли выгружать эту физ-зону после работы или мы еЄ оставл€ем как есть
      RECL_nzon_type = 1 !нужно инициализировать сразу же, потому что мы сразу будем работать с этим массивом
      RECL_FizMatCounter_GLOBAL = 0
      allocate(RECL_FizOldMatIds_GLOBAL(1:99999))
      allocate(RECL_FizNewMatIds_GLOBAL(1:99999))
      RECL_FizMatCounter_GLOBAL_R = 0
      allocate(RECL_FizOldMatIds_GLOBAL_R(1:999999))
      allocate(RECL_FizNewMatIds_GLOBAL_R(1:999999))
      allocate(RECL_FizStepLoaded(1:9999999))

      write(*, *) 'READ REPRORYV INPUT FILE ...'
      call ReadREPRORYVInputData
      write(*, *) 'READ REPRORYV INPUT FILE DONE!'
      re_i = system(trim(DelComm)//' '//inp_temp//' 1>>tmp 2>&1')
      re_i = system(trim(DelComm)//' '//inpRECL_temp//' 1>>tmp 2>&1')

      allocate(MassDataPu(1:MkCamp*nCamp))
      allocate(MassDataU(1:MkCamp*nCamp))
      allocate(PuVector(1:MkCamp*nCamp))
      allocate(UVector(1:MkCamp*nCamp))
      allocate(PuVector_LOADED(1:MkCamp*nCamp))
      allocate(UVector_LOADED(1:MkCamp*nCamp))
      do i=1,MkCamp*nCamp
        MassDataU(i)%mass_after = 0
        MassDataU(i)%mass_before = 0
        MassDataU(i)%mass_loaded = 0
        MassDataU(i)%mass_recycle_after = 0
        MassDataU(i)%mass_recycle_before = 0
        MassDataU(i)%mass_unloaded = 0

        MassDataPu(i)%mass_after = 0
        MassDataPu(i)%mass_before = 0
        MassDataPu(i)%mass_loaded = 0
        MassDataPu(i)%mass_recycle_after = 0
        MassDataPu(i)%mass_recycle_before = 0
        MassDataPu(i)%mass_unloaded = 0

        PuVector(i)%Pu236 = 0
        PuVector(i)%Pu238 = 0
        PuVector(i)%Pu239 = 0
        PuVector(i)%Pu240 = 0
        PuVector(i)%Pu241 = 0
        PuVector(i)%Pu242 = 0

        UVector(i)%U234 = 0
        UVector(i)%U235 = 0
        UVector(i)%U236 = 0
        UVector(i)%U238 = 0

        PuVector_LOADED(i)%Pu236 = 0
        PuVector_LOADED(i)%Pu238 = 0
        PuVector_LOADED(i)%Pu239 = 0
        PuVector_LOADED(i)%Pu240 = 0
        PuVector_LOADED(i)%Pu241 = 0
        PuVector_LOADED(i)%Pu242 = 0

        UVector_LOADED(i)%U234 = 0
        UVector_LOADED(i)%U235 = 0
        UVector_LOADED(i)%U236 = 0
        UVector_LOADED(i)%U238 = 0
      end do
    end if


    if (StepGlobal == 1) then
      RECL_nzon_type = 1 !все - единицы (исходные физ-зоны)
      do i=Nf+1,M1
        RECL_nzon_type(i)=3 ! ћ - это третий тип
      end do
      RECL_FizStepLoaded=1 !в первый момент времени все загружены одинаково в 1-й год
    end if

    !статус “¬—ки - вкл/выкл (если “¬—ки нет в активной зоне, то мы можем еЄ потом использовать)
    if (.not.allocated(RECL_TVSnum_Enabled)) then
      allocate(RECL_TVSnum_Enabled(1:maxNk))
      RECL_TVSnum_Enabled = 1  !первоначально все включены
    end if
end subroutine
