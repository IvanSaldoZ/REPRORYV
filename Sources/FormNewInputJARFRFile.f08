!��������� ����� ������� ���� ��� JARFR ��� ������� ������� ��������� (��������� �� ���������� ������� ��������� �����)
subroutine FormNewInputJARFRFile
    use CommonModJARFR
    !������� ����� ����� � ����� ���������� ����� (�.�. ��� �������, ������� ��� �� ���� ������������)
    integer AddNewTVSType, AddNewTVSUniqueType
    !������� ������ ���������� � ���������� ������� �����, ����������� �� ���������� ����
    integer MinusOneTVSCount
    !�� ���������� ����� ���������� (���������� ���) � �� ���������� �������, ������� �� ������ �������� ��� �������� ����������� ���������� � ����� ����
    integer AddMatsCount, AddMatsUniqueCount
    !�� ���������� ����� � ���������� ���������� (���������� ���), ������� �� ������ �������� ������ ����������� ���������� �� ���������� ����
    integer AddMatsPreCount, AddMatsPreUniqueCount
    !�����/������ ���-�� ���
    integer NewMatsCount, OldMatsCount
    !������ ������� ���, ������� �� ����� ���������, ������ ����� ������� ���
    integer, allocatable :: RECL_TVSnumToReload(:), RECL_TVSnumToReload_TEMP(:), RECL_TVSnumCaption(:)
    !������ ������� ���, ������� �� ���� ��������� �� ����������� ����
    integer, allocatable :: RECL_TVSnumUnloaded(:), RECL_TVSnumUnloaded_TEMP(:)
    !������ ������� ��������, ������� �� ����� ���������
    integer, allocatable :: RECL_FIZids(:), RECL_FIZids_BIG(:), RECL_FIZids_TEMP(:)
    !������ ������� ��������, ������� ������������� ���, ������� �� ����� ��������� (������������ ����� RECL_FIZids <-> RECL_FizNewIds)
    integer, allocatable :: RECL_FizNewIds(:)
    !������ ������� ��������, ������� ���� ��������� �� ���������� ����
    integer, allocatable :: RECL_FIZolds(:), RECL_FIZolds_TEMP(:), RECL_FIZolds_BIG(:)
    !������ ������� ��������, ������� ������������� ���, ������� ���� ��������� (������������ ����� RECL_FIZolds <-> RECL_FizNewOld)
    integer, allocatable :: RECL_FizNewPre(:)
    !���� �� � ������� ����� �������
    logical isInArr
    !���������� ������ KT
    integer, allocatable :: KTx2(:,:), KTx2_ORIGINAL(:,:), KTx2_TEMP(:,:)
    !������, � ������� �������� ������ �������� ��� ���� �������
    integer newIndex
    !���������� ������ AD - ������������ ������� �� ������� ��������
    real, allocatable :: ADx2(:,:)
    !���� �� ��������� �������������� ����� ����� ��� ����������� �������������
    logical isFreeToUse
    !����� ������ �����/���������� ���� ��� ���������� �������������
    integer newTVSindex, newFIZindex
    !������ ��� ������� ������ �������� NK � KT
    character*50 :: FMT2, FMT3
    !������� �� ������ ���� � �������� ���� ��� � ����� ������������
    integer, allocatable :: FizZoneStatus_TEMP(:)
    !��� ������ �����
    character*11 Str11
    character*250 Str250
    !��� ����� � ���������� ��������������
    character*250 curFile_CONC
    !����� �������� ����
    integer NumZonIn, NumZonFound
    !���������� ������������� �������
    integer IsotID
    !��������� ������������
    real ConcIn
    !����� ��������� ������ ����� ������� Tem
    real, allocatable :: Tem_Copy(:)
    !��������� ����� ������� Ntnz
    integer, allocatable :: Ntnz_temp(:)
    !������ ��� ��������� ������
    character*6 AddedStr, AddEndOfOutputFile

    re_i = system('copy '//inp_new//' '//trim(inp_new_to_run)//'  1>>tmp 2>&1')
    re_i = system(trim(DelComm)//' '//inp_new//'  1>>tmp 2>&1')
    open(unt1, file = trim(inp_new_to_run), status = 'OLD')
    !�������� ����� ��������� �������
    Jupbn = 1
    !����� �� ����������� ������ ����  ����������� � �������� �� ���� ������������� ������ ������� � ��������
    T1Recl = TCamp + TDown
    !����� ���-�� ������� ���������/��������/�����������
    T=(TCamp + TDown + (nVRH+nRecl)*T1Recl)
    !�� ������� ����� ��������
    !NDTnum = 1  !���-�� ����������, �� ������� �� ��������� ����� ������ �������� (����� � ���� ������ �������� Keff)
    NDT = NDTnum + 2 !5 ���������� �� �������� � 2 - ��� ��������� ������ �� �������� � �����������
    !�������� ������ ��� ��������
    deallocate(Jprn)
    deallocate(Jdpt)
    deallocate(T_Usr)
    deallocate(PWM)
    allocate(Jprn(1:NDT))
    allocate(Jdpt(1:NDT))
    allocate(T_Usr(1:NDT))
    allocate(PWM(1:NDT-1))
    !����� �� �����
    do i=1,NDTnum !��������� ������ �������� �������
      T_Usr(i) = TCamp/NDTnum  !����� ������ �� �������� (330 ���/���-�� ����������)
      if (i < NDTnum) then
        PWM(i) = Pw  !�������� ����� �� ��������
      end if
    end do
    !T_Usr(1) = TCamp  !����� ������ �� �������� (330 ���)
    T_Usr(NDTnum + 1) = TDown  !����� �������� (30 ���)
    T_Usr(NDTnum + 2) = (nVRH+nRecl)*T1Recl  !����� �������� + ����������� (2+1 ���) �� ���������������� ���������
    !�������� �� �����
    PWM(NDTnum) = 0.0000001  !�������� �������� (30 ���)
    PWM(NDTnum+1) = 0.0000001  !�������� �������� + ����������� (2+1 ���) �� ���������������� ���������
    Jprn = 3
    Jpri = 2 !����� ��� ����������� ������������
    JDPT = 0
    NKR = 1  !����� ��� ���������� Kr
    NKV = 1  !����� ��� ���������� ������������ ���������������

    !===============================
    !������ ����� �������!!!!
    !===============================
    !��������� �������� KT � NK
    !�������, ����� ������ ��� ��� ����� �����������
    i = 1
    AddNewTVSType = 1
    AddNewTVSUniqueType = 1
    allocate(RECL_TVSnumToReload_TEMP(1:99999)) !�������� ��������� ������
    RECL_TVSnumToReload_TEMP = -1
    !�������, ������ ����� ���, ������� ����� ��������� �� �� � ����� �������������
    do while (i <= NkDimention)
      !��� ������� �������� �� ������ ���������� ������� ������� ��� ����������
      !if (isTEST >= 2) write(333, *) 'NkRecl(',i,')=',NkRecl(i);
      !���� ��� ������� ���, ��:
!      if (NkRecl(i) > 0) then
      if (NkRecl(i) == MkCampCounter) then
        !������� ��������� ���� �� ��� ����� ����� ���� ��� � �������
        isInArr = .false.
        j = 1
        do while (j <= AddNewTVSType)
          if (RECL_TVSnumToReload_TEMP(j) == Nk(i)) then
            isInArr = .true.
          end if
          j = j + 1
        end do
        !���� � ������� ����� ������� ���� ���, �� ���������
        if (isInArr.eqv..false.) then
          !RECL_TVSnumToReload = RECL_TVSnumToReload(1:AddNewTVSType)
          RECL_TVSnumToReload_TEMP(AddNewTVSType) = Nk(i)
          !if (isTEST >= 2) write(333, *) 'RECL_TVSnumToReload(',AddNewTVSType,')=',RECL_TVSnumToReload(AddNewTVSType)
          AddNewTVSType = AddNewTVSType + 1
        end if
      end if
      i = i + 1
    enddo
    !���������� ���-�� ����������� ������� ��� �� �����
    AddNewTVSType = AddNewTVSType - 1
    allocate(RECL_TVSnumToReload(1:AddNewTVSType))
    do i=1,AddNewTVSType
      RECL_TVSnumToReload(i)=RECL_TVSnumToReload_TEMP(i)
    end do
    deallocate(RECL_TVSnumToReload_TEMP) !������� ��������� ������ �� ������
    AddNewTVSUniqueType = AddNewTVSType !(���� ��� ��� �����)
    !��������� ������ RECL_TVSnumToReload (����� �� ������� ���� ���)
    call sort(RECL_TVSnumToReload, AddNewTVSType)
    write(333, *) 'Added new TVS on this step=',AddNewTVSType


    !�������, ����� ��� ���� ��������� �� ���������� ���� � ���������� �� ������ ��� �������� �����/�������������� �����
    if (StepGlobal > 1) then
      i = 1
      MinusOneTVSCount = 1  !���������� �����, ����������� �� ���������� ����
      !MinusOneTVSUniqueCount = 1  !���������� ���������� ����� ������� �����, ����������� �� ���������� ����
      allocate(RECL_TVSnumUnloaded_TEMP(1:999999))
      RECL_TVSnumUnloaded_TEMP = -1 !��������������
      !�������, ������ ����� ���, ������� ���� ��������� �� �� � ����� ���������� �������������
      do i=1,NkDimention
        !��� ������� �������� �� ������ ���������� ������� ���������� ��� ���������� (�������������)
        if (MkCampCounter == 1) then
          j = MkCamp
        else
          j = MkCampCounter - 1
        end if
        if (NkRecl(i) == j) then
          !������� ��������� ���� �� ��� ����� ����� ���� ��� � �������
          isInArr = .false.
          do j = 1,MinusOneTVSCount
            if (RECL_TVSnumUnloaded_TEMP(j) == Nk(i)) then
              isInArr = .true.
            end if
          end do
          !���� � ������� ����� ������� ���� ���, �� ���������
          if (isInArr.eqv..false.) then
            !RECL_TVSnumUnloaded = RECL_TVSnumUnloaded(1:MinusOneTVSCount)
            RECL_TVSnumUnloaded_TEMP(MinusOneTVSCount) = Nk(i)
            !if (isTEST >= 2) write(333, *) 'RECL_TVSnumUnloaded(',MinusOneTVSCount,')=',RECL_TVSnumUnloaded(MinusOneTVSCount)
            MinusOneTVSCount = MinusOneTVSCount + 1
          end if
        end if
      end do
      !���������� ���-�� ����������� ������� ��� �� �����
      MinusOneTVSCount = MinusOneTVSCount - 1
      !AddNewTVSUniqueType = MinusOneTVSCount   !(���� ��� ��� �����)
      !MinusOneTVSUniqueCount = MinusOneTVSCount   !(���� ��� ��� �����)
      allocate(RECL_TVSnumUnloaded(1:MinusOneTVSCount))  !�������� ��������� ������
      do i=1,MinusOneTVSCount
        RECL_TVSnumUnloaded(i) = RECL_TVSnumUnloaded_TEMP(i)
      end do
      deallocate(RECL_TVSnumUnloaded_TEMP)
      !��������� ������ RECL_TVSnumToReload (����� �� ������� ���� ���)
      call sort(RECL_TVSnumUnloaded, MinusOneTVSCount)
      write(333, *) 'Old TVS number from previous step=',MinusOneTVSCount
      write(333, *) 'TVS from previous step that were unloaded(RECL_TVSnumUnloaded): ',RECL_TVSnumUnloaded
      if (isTEST >= 2) write(333, *) 'RECL_TVSnumUnloaded=',RECL_TVSnumUnloaded
    end if


    !��������� ������� ��� ���������� ��� ��� ���� ������� ���/���������� ��� � �������� ���� ����� �������� ����� �� ���� ���� � ��������� (������ ����) ��������������� ������, ���� ��� ������ �� ������������
    !�������������� ����������
    deallocate(RECL_TVSnum_Enabled)
    allocate(RECL_TVSnum_Enabled(1:maxNk+AddNewTVSUniqueType))
    write(333, *) 'CHECKING FOR THE EXISTANCE OF THE TVS IN THE CORE...'
    !if (isTEST >= 2) write(333, *) 'CHECKING FOR THE EXISTANCE OF THE FIZ ZONES IN THE CORE'
    RECL_TVSnum_Enabled = 1
    do i=1,AddNewTVSType !��� ������� ������ ���� ���
      !do k=1,MkCamp
        newTVSindex = RECL_TVSnumToReload(i)
        !if (isTEST >= 2) write(333, *) 'newTVSindex=',newTVSindex
        isInArr = .false.
        isTVSExists: do j=1,NkDimention
         ! if (isTEST >= 2) write(333, *) 'Nk(',j,')=',Nk(j)
          if (Nk(j) == newTVSindex) then
            !��������� �� ���, ������� �� ����� �������� �� ���� ���� (��� ����� ��� ����, ����� ������ ���������� ��� ����� ������ ������ �����)
!            if (NkRecl(j).ne.k) then
            if (NkRecl(j).ne.MkCampCounter) then
              isInArr = .true. !����� ����� ��� ���� � �������
            end if
            !EXIT isTVSExists !����� �� �����
          end if
        end do isTVSExists
        !���� �� ������ ���� ��� �� ��������� � �������, �� ����� ���������, ���� �� ��� ����� ���-����, ������� ������ ����� �� ����������� (����� �������� �� ������������, ���� ��� ������ �� ������������)
        if (isInArr.eqv..false.) then
          !���������, ������� �� ��� ���� ����� � ������ ������������� �����
          RECL_TVSnum_Enabled(newTVSindex)=0
          !if (isTEST >= 2) write(333, *) 'TVS TYPE IS NOT FOUND IN ARRAY, finding the existance of fiz zones in this TVS'
          write(333, *) 'TVS TYPE',newTVSindex,' IS NOT FOUND IN ARRAY. Disable it.'
        end if
      !end do
    end do


    !��������� ������ �������� ��� ����� ����� ���
    allocate(RECL_TVSnumCaption(1:AddNewTVSType)) !�������� ��������� ������ ��� ������� ����� ������� ���
    !RECL_TVSnumCaption=RECL_TVSnumCaption(1:AddNewTVSType) !�������� ��������� ������ ��� ������� ����� ������� ���
    i = 1
    n = 0
    do while (i <= AddNewTVSType)
      isFreeToUse = .false.
      newTVSindex = -1
      !�������� ����� �� ������� ����� �����, ����� ������������ ��� ��� ����������� ����� �������� (��� ������ ������ ����� 1-� �������� ��������)
      FindFreeTVS: do j=1,maxNk
        if (RECL_TVSnum_Enabled(j)==0) then
          isFreeToUse = .true.
          write(333, *) 'TVS TYPE',j,' IS USED FOR NEW MATERIALS. ENABLE IT'
          newTVSindex = j
          RECL_TVSnum_Enabled(j) = 1
          exit FindFreeTVS
        end if
      end do FindFreeTVS
      !������ ������ ����, ����� ������������ ����� ����� �����, �������� ��� �� ������ �����
      if (isFreeToUse.eqv..false.) then
        RECL_TVSnumCaption(i) = maxNk + i - n
        !write(333,*) 'RECL_TVSnumCaption(i)=',RECL_TVSnumCaption(i)
      else
        RECL_TVSnumCaption(i) = newTVSindex
        AddNewTVSUniqueType = AddNewTVSUniqueType - 1 !���� �� ���������� ��� ������ �����, �� ���������� ������� � ��� ���������
        n = n + 1
      end if
      i = i + 1
    end do
    write(333, *) 'Added unique type of TVS: ',AddNewTVSUniqueType
    write(333, *) 'NEW TVS CAPTION:',RECL_TVSnumCaption
    !���������� ���������� ������� NK � ������� ���� - �� ������
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


    !�������� � NK ������ ��� �� ��, ������� ����� ��������� � ����� ����� ���� ��� �����������
    do i=1,NkDimention
    !������� �������� ����� ��� �����
      !do k=1,MkCamp
        !��� ������� �������� �� ������ ���������� ������� ������� ��� ����������
        !if (NkRecl(i) == k) then
         if (NkRecl(i) == MkCampCounter) then
          do j=1,AddNewTVSType
            !������� ����� �������, �������������� ������ ���� ���
            if (RECL_TVSnumToReload(j) == Nk(i)) then
              Nk(i) = RECL_TVSnumCaption(j)
            end if
          end do
        end if
      !end do
    end do

    !���������� ���������� ������� NK � ������� ����
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

    !������ ��������� ������ �� KT, ����� ����� ���� � ��� ����������
    k = 1 !����������� ������ ��� ����������� ����������
    allocate(KTx2(1:maxNk, 1:MaxNSLZ))
    allocate(KTx2_ORIGINAL(1:maxNk, 1:MaxNSLZ)) !��������� �������� ��� ���������
    do i=1,maxNk
      do j=1,MaxNSLZ
        KTx2(i,j) = KT(k) !��������� ���������� ������
        KTx2_ORIGINAL(i,j) = KT(k) !��������� �������� �� ������
        k = k + 1
      end do
    end do

    !���������� ���������� ������� KT � ������� ���� - �������� ������
    write(333, *) '----'
    write(333, *) 'KT - ZONES ORIGINAL'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk
      write(333, trim(FMT2)) KT((i-1)*MaxNSLZ+1:i*MaxNSLZ)
    end do


    !���������� ����� ������ �������� ��� ���������� �� ���� ����

    !������ ������� � KT �� ������ ��������, ������� ��� ����� ����������� � ������� �� � ������ RECL_FIZids_TEMP
    write(333, *) '----'
    write(333, *) 'Finding in KT the fiz zones that need to be recycled on this step...'
    write(333, *) 'TVS that will be reloaded on this step (RECL_TVSnumToReload): ',RECL_TVSnumToReload
!   if (isTEST >= 2) write(333, *) 'RECL_TVSnumToReload= ',RECL_TVSnumToReload
    k = 1 !����� ������������� ����������
    allocate(RECL_FIZids_TEMP(1:99999))
    RECL_FIZids_TEMP = -1
    do j=1,AddNewTVSType
      do i=1,maxNk
        !��������� ������ �� ������, ������� ��� ����� (= ������ �����)
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
    !��� ����� ������ ���������� �������, ������� �� ������� TEMP ��������� ������ ���������� ������
    allocate(RECL_FIZids_BIG(1:99999))
    RECL_FIZids_BIG = -1
    n = 1
    do i=1,k-1
      !�������, ���� �� ����� ������ � �������
      isInArr =.false.
      do j=1,n
        if (RECL_FIZids_BIG(j)==RECL_FIZids_TEMP(i)) then
          isInArr =.true.
        end if
      end do
      if (isInArr.eqv..false.) then !���� ������ ������� ��� ��� � �������, �� ���������
        RECL_FIZids_BIG(n) = RECL_FIZids_TEMP(i)
        n = n + 1
      end if
    end do
    AddMatsCount = n - 1   !���������� ����� ����� ����������
    allocate(RECL_FIZids(1:AddMatsCount))
    do i=1,AddMatsCount
      RECL_FIZids(i) = RECL_FIZids_BIG(i)
    end do
    deallocate(RECL_FIZids_BIG)
    !RECL_FIZids=RECL_FIZids(1:AddMatsCount)
    call sort(RECL_FIZids, AddMatsCount)
    if (isTEST >= 2) write(333, *) 'RECL_FIZids= ',RECL_FIZids
    AddMatsUniqueCount = AddMatsCount !�� ��������� ������ ������� ��� �����
    write(333, *) '-------'
    write(333, *) 'Added new materials count for recycling (AddMatsCount)=',AddMatsCount
    write(333, *) 'Founded materials that need to be recycled on this step (RECL_FIZids)=',RECL_FIZids
    !��������� ������ ������� ����� ��������, ��������������� �����������, ����� ����� ���� �������� �� ���� ����� �������
    write(333, *) 'Generate new numbers of fiz zones for the recycling...'
    allocate(RECL_FizNewIds(1:AddMatsCount)) !�������� ������ ��� �������, � ������� ����� ��������� ���� ������ ����� ��������
    n = 1
    do i=1,AddMatsCount
      !���� ��������� ������
      isFreeToUse = .false.
      newFIZindex = -1
      FindFreeFiz: do j=1,M1
        if (RECL_nzon_type(j)==5) then !���� ����� ��������� �����, �� �������� ���
        !if (RECL_nzon_type(j)==444) then !���� ����� ��������� �����, �� �������� ���
          newFIZindex = j
          write(333, *) 'Founded not used material',j,'. Use it for farther recycling.'
          isFreeToUse = .true.
          RECL_nzon_type(j) = 2   !�� ��������� ���� ��� ���� ����� ������������
          exit FindFreeFiz
        end if
      end do FindFreeFiz
      !���� ��������� ������� ���, �� ��������� ����� ��������
      if (isFreeToUse.eqv..false.) then
        RECL_FizNewIds(i) = Nf + n
        n = n + 1
      else !���� ��������� ������ ���� - �� �������� ���
        RECL_FizNewIds(i) = newFIZindex
        AddMatsUniqueCount = AddMatsUniqueCount - 1 !������� ����� ���������� ������ ����� �� ������� ������
      end if
    end do
    !������ ������ ����� ��� �� 2-�� - �� ����� ����� ���� ������ ���� ����� ������������

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
      RECL_nzon_type(j) = 2   !�� ��������� ���� ��� ���� ����� ������������
    end do
    !if (isTEST >= 2) write(333, *) 'RECL_FizNewIds= ',RECL_FizNewIds
    !if (isTEST >= 2) write(333, *) 'AddMatsUniqueCount= ',AddMatsUniqueCount
    !��� ����� ���������� �������� �� ������
    !RECL_Fiz_Enabled = RECL_Fiz_Enabled(1:(M1+AddMatsUniqueCount))
    !��������� ���, �� ������� ���� ��������� ������ ����
    !RECL_FizStepLoaded = RECL_FizStepLoaded(1:(M1+AddMatsUniqueCount))
    RECL_FizStepLoaded((M1+1):(M1+AddMatsUniqueCount)) = StepGlobal
    do i=1,(M1+AddMatsUniqueCount)
      do j=1,AddMatsCount
        if (i == RECL_FizNewIds(j)) then
!          RECL_FizStepLoaded(i) = StepGlobal
        end if
      end do
    end do

    !���������� ����� ������ ��� ����������� ������� ��������
    AddMatsPreCount = 0
    AddMatsPreUniqueCount = 0
    if (StepGlobal > 1) then
      !������� � KT �� ������ ��������, ������� ���� ��������� �� ���������� ���� � ������� �� � ������ RECL_FIZolds_TEMP
      write(333, *)
      write(333, *) '----'
      write(333, *) 'Finding in KT the fiz zones that were unloaded on the previous step...'
      k = 1 !����� ������������� ����������
      allocate(RECL_FIZolds_TEMP(1:99999))
      RECL_FIZolds_TEMP = -1
      !allocate(RECL_FIZolds_TEMP(1:k))
      !do m=1,AddNewTVSType
      do m=1,MinusOneTVSCount
        do i=1,maxNk
          !��������� ������ �� ������, ������� ��� ����� (= ������ �����)
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
      !��� ����� ������ ���������� �������, ������� �� ������� TEMP ��������� ������ ���������� ������
      n = 1
      allocate(RECL_FIZolds_BIG(1:99999))
      RECL_FIZolds_BIG = -1
      do i=1,k-1
        !�������, ���� �� ����� ������ � �������
        isInArr =.false.
        do j=1,n
          if (RECL_FIZolds_BIG(j)==RECL_FIZolds_TEMP(i)) then
            isInArr =.true.
          end if
        end do
        if (isInArr.eqv..false.) then !���� ������ ������� ��� ��� � �������, �� ���������
          RECL_FIZolds_BIG(n) = RECL_FIZolds_TEMP(i)
          n = n + 1
        end if
      end do
      AddMatsPreCount = n - 1 !���������� ����� ����� ���������� ��� �������� � ��� ����� ������������
      allocate(RECL_FIZolds(1:AddMatsPreCount))
      do i=1,AddMatsPreCount
        RECL_FIZolds(i) = RECL_FIZolds_BIG(i)
      end do
      deallocate(RECL_FIZolds_BIG)
      !RECL_FIZolds=RECL_FIZolds(1:AddMatsPreCount)
      call sort(RECL_FIZolds, AddMatsPreCount)
      if (isTEST >= 2) write(333, *) 'RECL_FIZolds= ',RECL_FIZolds
      AddMatsPreUniqueCount = AddMatsPreCount !�� ��������� ������ ������� ��� �����
      write(333, *) 'Added new materials count for previously unloaded (AddMatsPreCount)=',AddMatsPreCount
      write(333, *) 'Founded materials that were unloaded on previously step (RECL_FIZolds)=',RECL_FIZolds
      !��������� ��� ����������� ����������� ������ ���
      do i=1,AddMatsPreCount
        j = RECL_FIZolds(i)
        RECL_nzon_type(j) = 0
      end do
      !��������� ������ ������� ����� ��������, ��������������� �����������, ����� ����� ���� �������� �� ���� ����� �������
      write(333, *) 'Generate new numbers of fiz zones for the previously unloaded materials...'
      allocate(RECL_FizNewPre(1:AddMatsPreCount)) !�������� ������ ��� �������, � ������� ����� ��������� ���� ������ ����� ��������
      n = 1
      do i=1,AddMatsPreCount
        !���� ��������� ������
        isFreeToUse = .false.
        newFIZindex = -1
        FindFreeFizPre: do j=1,M1
          if (RECL_nzon_type(j)==5) then !���� ����� ��������� �����, �� �������� ���
          !if (RECL_nzon_type(j)==444) then !���� ����� ��������� �����, �� �������� ���
            newFIZindex = j
            write(333, *) 'Founded not used material',j,'. Use it for loading new zone.'
            isFreeToUse = .true.
            RECL_nzon_type(j) = 1   !������ ������� ����� ����
            exit FindFreeFizPre
          end if
        end do FindFreeFizPre
        !���� ��������� ������� ���, �� ��������� ����� ��������
        if (isFreeToUse.eqv..false.) then
          RECL_FizNewPre(i) = Nf + n     + AddMatsUniqueCount  !� ������ ����, ��� �� ��� �������� ��������� ��� �������
          n = n + 1
        else !���� ��������� ������ ���� - �� �������� ���
          RECL_FizNewPre(i) = newFIZindex
          AddMatsPreUniqueCount = AddMatsPreUniqueCount - 1 !������� ����� ���������� ������ ����� �� ������� ������
        end if
      end do
      !������ ������ ����� ��� �� 1-�� - ������� ����� ����
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
        RECL_nzon_type(j) = 1   !������ ������� ����� ����
      end do
      !if (isTEST >= 2) write(333, *) 'RECL_FizNewPre= ',RECL_FizNewPre
      !if (isTEST >= 2) write(333, *) 'AddMatsPreUniqueCount= ',AddMatsPreUniqueCount
      write(333, *) 'New captions of new materials for unloaded previously zones (RECL_FizNewPre): ',RECL_FizNewPre
      write(333, *) 'New UNIQUE material count for unloaded previously zones (AddMatsPreUniqueCount): ',AddMatsPreUniqueCount
    end if
    if (StepGlobal > 1) then
      !��������� ������ ��� ����������� ����� � ������ ������ ����������� ��� � ����������� ���������� ������
      do i=1,AddMatsPreCount
        !��������� � ���������� ����������, ����� ����� ����� ���� ��������������� ��� �����������
        RECL_FizOldMatIds_GLOBAL(RECL_FizMatCounter_GLOBAL+i)=RECL_FIZolds(i)
        RECL_FizNewMatIds_GLOBAL(RECL_FizMatCounter_GLOBAL+i)=RECL_FizNewPre(i)
      end do
      !if (isTEST >= 1) write(333, *) '  RECL_FizOldMatIds_GLOBAL=',RECL_FizOldMatIds_GLOBAL(1:RECL_FizMatCounter_GLOBAL)
      !if (isTEST >= 1) write(333, *) '  RECL_FizNewMatIds_GLOBAL=',RECL_FizNewMatIds_GLOBAL(1:RECL_FizMatCounter_GLOBAL)
      RECL_FizMatCounter_GLOBAL = RECL_FizMatCounter_GLOBAL + AddMatsPreCount
      !if (isTEST >= 1) write(333, *) '  RECL_FizMatCounter_GLOBAL=',RECL_FizMatCounter_GLOBAL
    end if


    !������ ������ ��������� ��� �� 3-�� - ��������������� ���������
    do i=1,M1+AddMatsUniqueCount   + AddMatsPreUniqueCount
      if (i > Nf + AddMatsUniqueCount   + AddMatsPreUniqueCount) then
        RECL_nzon_type(i) = 3   !��������������� ��������
      end if
    end do

    !�������� � KT ��� ����������� �������, �������� � ������� ������ ���-�� ����� �������� AddMatsUniqueCount
    write(333, *) 'Adding numbers for constructive materials (KM) fiz zones...'
    do i=1,maxNk
      do j=1,MaxNSLZ
        if (KTx2(i,j) > Nf) then
          KTx2(i,j) = KTx2(i,j) + AddMatsUniqueCount + AddMatsPreUniqueCount
        end if
      end do
    end do



    !�������� ��� �������� �� � ����� �������
    !RECL_FizStepLoaded = RECL_FizStepLoaded(1:M1+AddMatsUniqueCount+AddMatsPreUniqueCount)
    do i=Nf+1,M1
      newIndex = i + AddMatsUniqueCount+AddMatsPreUniqueCount
      RECL_FizStepLoaded(newIndex) = RECL_FizStepLoaded(i)
    end do
    !������ ����� ���� �������� ��� ����� ��� ��������
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


    !���������� ���������� ������� KT � ������� ����
    write(333, *) '----'
    write(333, *) 'KT - ZONES ADDED KM MATERIALS'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk
      write(333, trim(FMT2)) KTx2(i,1:MaxNSLZ)
    enddo



    !�������� � ����� KT ���� �����, ����� ����� ��������������� ������� �� ������
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
        !��������� ������ �� ������, ������� ��� ����� (= ������ �����)
        if (i == RECL_TVSnumToReload(k)) then
          do j=1,MaxNSLZ
            newIndex = RECL_TVSnumCaption(k)  !������������ ����� ������� � ������ �������� ���
            KTx2(newIndex,j) = KTx2(i,j)
          end do
        end if
      end do
    end do
    if (isTEST >= 2) write(333, *) 'KTx2_EXTENDED= ',KTx2


    !���������� ���������� ������� KT � ������� ����
 !   write(333, *) '----'
 !   write(333, *) 'KT - ZONES EXTENDED'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk+AddNewTVSUniqueType
!      write(333, trim(FMT2)) KTx2(i,1:MaxNSLZ)
    enddo


    !������, ��� ����� ������� � KT-��� �������� ������ ����� ��������� �������� �� ����� ������ ��������, ������� ����� �� ����� ������������
    write(333, *) '----'
    write(333, *) 'Change numbers of material in the KT map to new materials numbers'
    do i=1,(maxNk+AddNewTVSUniqueType)
      !����� �������� ��������� ������ � ������������� ������, ������� ������� ���������, ��������� �� ������ ����� � �������������?
      isInArr = .false. !������� �������, ��� �� ���������
      do j=1,AddNewTVSType
        if (RECL_TVSnumCaption(j)==i) then !��, ���� ����� �������������
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
      !�������� ������ �������� � ������������� ��� �� ����� ������ ��������
      write(333, *) '----'
      write(333, *) 'Change numbers of material of previoulse unloaded zones in the KT map to new materials numbers'
      do i=1,maxNk
        !����� �������� ��������� ������ � ������������� ������, ������� ������� ���������, ��������� �� ������ ����� � �������������?
        isInArr = .false. !������� �������, ��� �� ���������
        do j=1,MinusOneTVSCount !��� ���� ���������� ����������� �� ���������� ���� �����
          if (RECL_TVSnumUnloaded(j)==i) then !���� ����� ������ ������������� �����... � ��, ���� ��� ��
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
    !��������� �������� KT � NK ���������
    !===============================




    !===============================
    !��������� ������� �������� AD
    !������ ��������� ������ �� AD, ����� ����� ���� � ��� ����������
    k = 1 !����������� ������ ��� ����������� ����������
    !allocate(ADx2(1:M1, 1:Ner1))
    allocate(ADx2(1:(M1+AddMatsUniqueCount    + AddMatsPreUniqueCount), 1:Ner1))
    do i=1,M1
      do j=1,Ner1
        ADx2(i,j) = Ad(k) !��������� ���������� ������
        k = k + 1
      end do
    end do
    !�������� ��������� �� � �����
    newIndex = -1
    !ADx2=ADx2(1:(M1+AddMatsUniqueCount    + AddMatsPreUniqueCount), 1:Ner1)
    do i=Nf+1,M1
      do j=1,Ner1
        newIndex = i + AddMatsUniqueCount      + AddMatsPreUniqueCount
        ADx2(newIndex,j) = ADx2(i,j) !��������� ���������� ������
        !if (isTEST >= 2) write(333, *) 'ADx2_OLD(',i,',',j,')= ',ADx2(i,j)
        !if (isTEST >= 2) write(333, *) 'ADx2_NEW(',newIndex,',',j,')= ',ADx2(newIndex,j)
      end do
    end do
    !�������� ������� ��������� �������� � ����� �����, �������� ����� �������
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
            ADx2(newIndex,j) = ADx2(i,j) !��������� ���������� ������
            !if (isTEST >= 2) write(333, *) 'ADx2_OLD(',i,',',j,')= ',ADx2(i,j);
            !if (isTEST >= 2) write(333, *) 'ADx2_NEW(',newIndex,',',j,')= ',ADx2(newIndex,j);
          end do
          !if (isTEST >= 2) write(333, '(a,i4,a,i4)') '  Copy concentrations of recycled Zone ',RECL_FIZids(n),' => ',RECL_FizNewIds(n)
          !��������� � ���������� ����������, ����� ����� ����� ���� ��������������� ��� �����������
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



    !��������� ������ ����� ���� �������������� ������������
    if (StepGlobal > 1) then
      !�������� ������������ ��������� ��� ����� ������ �� ����� ������ ���� � �������� ������
      if (StepGlobal.LE.(nVRH+nRecl+1)) then
        AddedStr = AddEndOfOutputFile(1) !������� ��� ���������� ����� �� ��������� ������ (1-� - 1load)
        curFile_CONC = 'recycle\1stp\1_conc'//trim(AddedStr)//'.txt' !������������
        if (isTEST >= 2) write(333, *) 'curFile_CONC = '//trim(curFile_CONC )
        open (55,file=trim(curFile_CONC),status='old')
      end if
      n = 1
      newIndex = -1
      do m=1,AddMatsPreCount
        newIndex = RECL_FizNewPre(m)
        if (isTEST >= 2) write(333, *) 'ZONE ',newIndex,' IS PLACE TO SET NEW CONC FOUND!'
        if (StepGlobal > (nVRH+nRecl+1)) then
          !���� ��� ����� ��������� �����������
          if (isTEST >= 2) write(333, *) 'Loading recycling TVS into zone ',newIndex
          do j=1,Ner1 !��� ������� ������� ������ ��� ������
            ADx2(newIndex,j) = IsotReclConc(j) !����������� ������������ �������� ����� �����������
          end do
          if (isTEST >= 2) write(333, *) 'Loading recycling TVS into zone ',newIndex,'... DONE!'
        else
          !�������� ������������ ����������� ������� - �Ѩ ����
          !���� ��� ����� �� ������ ��� �������� �����������, �� ����� �������� �������� �������, ������� ����� ����� � �������� ����� � ��������������
          if (isTEST >= 2) write(333, *) 'Loading initial TVS'
          do j=1,RECL_FizMatCounter_GLOBAL
            if (newIndex == RECL_FizNewMatIds_GLOBAL(j)) then
              if (StepGlobal == 2) then
                NumZonFound = RECL_FizOldMatIds_GLOBAL(j)  !�� ������ ����� ����� ������������ ����� ������ ��������, ������ ������ ����� ���� ������ �� ����� �� �������������
              else
                !���� �������� ����� ����� ������� �������, ������������� ��� ����������� ���
                do k=1,RECL_FizMatCounter_GLOBAL_R
                  if (RECL_FizNewMatIds_GLOBAL_R(k) == RECL_FizOldMatIds_GLOBAL(j)) then
                    NumZonFound = RECL_FizOldMatIds_GLOBAL_R(k)  !�������, ����� ���� ��������������� ������
                  end if
                end do
              end if
            end if
          end do
          !���� �������� ������������ ��� ���� ����
          ios = 0
          !iostat:
            ! = -1 error: end of file
            ! = -2 error: end of record
          !������ ���-�� ����� � �����
          if (isTEST >= 2) write(333, *) '-----------Finding zone ',NumZonFound
          do while (ios /= -1)
            read(55, '(A)', advance = "yes", iostat = ios) Str250
            if (ios /= -1) then
              if (Str250(1:11).eq.'       ����') then
                read(Str250,'(a11,I5)') Str11, NumZonIn !      ����    1.
                read(55,*) ! TYPE:            1
                read(55,*) ! Loaded on Step 1
                if (NumZonIn == NumZonFound) then
                  if (isTEST >= 2) write(333, *) 'FOUND!'
                  do j=1,Ner1 !��� ������� ������� ������ ��� ������
                    read (55,'(i3.3, e14.5)') IsotID, ConcIn
                    ADx2(newIndex,j) = ConcIn !���� 1-� ��� 3-� ���, �� ��������� ��� �� ��������, ������� ��� �� ������ ���� ������
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
    deallocate(IsotReclConc)  !������ �� ������ ���� ��� �������������� ������������ �� �����������
    !if (isTEST >= 2) write(333, *) 'ADx2= ',ADx2;


    !��������� - ��������� ���������� ������ � ����������
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


    !���������� ���������� ������� KT � ������� ����
    write(333, *) '----'
    write(333, *) 'KT - ZONES END MAP'
    write(FMT2,*) MaxNSLZ
    FMT2 = '('//trim(FMT2)//'i4)'
    do i=1,maxNk+AddNewTVSUniqueType
      write(333, trim(FMT2)) KT((i-1)*MaxNSLZ+1:i*MaxNSLZ)
    enddo


    !��������� ����������� ���� ���������� ������� � ����� ��� (� ������� KT), ����� ��������� ���� ��� �����-�� �� ����������� � �����������
    if (isTEST >= 2) write(333, *) '     CHECK NOT USED MATERIAL ZONES IN KT ARRAY'
    n = 0 !���������� �������������� ����������
    do i=1,(M1+AddMatsUniqueCount     + AddMatsPreUniqueCount)
      isInArr = .false. !�� ��������� �������, ��� � � ������� ���
!      CheckCurFiz: do j=1,(maxNk+AddNewTVSUniqueType)
      CheckCurFiz: do j=1,((maxNk+AddNewTVSUniqueType)*MaxNSLZ)
!        do k=1,MaxNSLZ
!          if (KTx2(j,k) == i) then
           if (KT(j) == i) then
            isInArr = .true.  !���� �� ���� ����� ������ � �������, ������ ��������� ��� ���� - ��
            exit CheckCurFiz
          end if
!        end do
      end do  CheckCurFiz
      !��������� ������ ���� �� ������������, ���� � ��� ������ � �������
      if (.not.isInArr) then
        do k=1,Ner1
          ADx2(i,k) = 1E-10
          !if (isTEST >= 2) write(333, *) '        ADx2(',i,',',k,')=',ADx2(i,k)
        end do
        !��������� ���� �� ������� ������������, ���������� � ��� �������� �������������
        if (RECL_nzon_type(i).NE.0) then
          RECL_nzon_type(i)=5
          n = n + 1
        end if
        if (isTEST >= 1) write(333, *) 'Zone ',i,' has been excluded from the result file! //'
      end if
    end do
    !��������� ����������� ���� ���������� ��� � ����� �������� ���� (� ������ ������� KT, ���������� �� ������� NK), ����� ��������� ���� ��� �����-�� �� ����������� � �����������
    n = 0
    allocate(FizZoneStatus_TEMP(1:M1+AddMatsUniqueCount    + AddMatsPreUniqueCount))
    FizZoneStatus_TEMP = 0 !�� ��������� �������, ��� ��� ��� � �������� ����
    !��������� ��� ������ ��� � ������� �����������
    !write(333, *) '   Finding Nks'
    do i=1,NkDimention
      m = Nk(i)  !������� ����������� ����� �����
      !write(333, *) 'Nk(i)=',Nk(i)
      do j=1,MaxNSLZ !��������� ��� ������ ���������� � �����
        k = KTx2(m,j)
        FizZoneStatus_TEMP(k) = 1  !���� ����� ��������� ��� ������ - ����
        !write(333, *) 'FizZoneStatus_TEMP(',k,')=',FizZoneStatus_TEMP(k)
      end do
    end do
    do i=1,M1+AddMatsUniqueCount    + AddMatsPreUniqueCount
      !���� �����-�� �������� ����������� � �����������, �� ��������� ��� � ������� ������������
      if (FizZoneStatus_TEMP(i) == 0) then
        do k=1,Ner1
          ADx2(i,k) = 1E-10
        end do
        !��������� ���� �� ������� ������������, ���������� � ��� �������� �������������
        if (RECL_nzon_type(i).NE.0) then
          RECL_nzon_type(i)=5
!          RECL_FizNewIds())
          n = n + 1
        end if
        if (isTEST >= 1) write(333, *) 'Zone ',i,' has been excluded from the result file! \\'
      end if
    end do
    deallocate(FizZoneStatus_TEMP)


    !��������� � KT ������ �� ���������� ��������� ������� ��������
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


    !���������� ������������ ����� ������� � ������ ��������
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
    !��������� ������� �������� AD ���������
    !===============================


    !===============================
    !��������� ����������� ��� ���. ������� � ������������ � ��� �������
    allocate(Tem_Copy(1:M1))
    Tem_Copy = Tem
    deallocate(Tem)
    allocate(Tem(1:M1+AddMatsUniqueCount   + AddMatsPreUniqueCount))
    do i=1,M1
      Tem(i) = Tem_Copy(i)
    end do
    deallocate(Tem_Copy)

    !��������� � ����� ����������� ��� ��
    do i=1,(M1+AddMatsUniqueCount   + AddMatsPreUniqueCount)
      if ((i > Nf).AND.(i<=M1)) then
        newIndex = i + AddMatsUniqueCount    + AddMatsPreUniqueCount
        Tem(newIndex) = Tem(i)
      end if
    end do
    !��������� ����������� ������������� ��������
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
    !��������� ����� ����������� - �����
    !===============================


    !===============================
    !������������� ��� ��������� ������� �����
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Count of assebmlies to by recycled on this step (AddNewTVSType)= ',AddNewTVSType
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Count of new numbers of assebmlies in the Core (AddNewTVSUniqueType)= ',AddNewTVSUniqueType
    !�� ���������� ���������� (���������� ���), ������� �� �������� ��� ����� ���������� �� ��������� ����
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Materials quantity on current step to be recycled (AddMatsCount) = ',AddMatsCount
    !�� ���������� ���������� (���������� ���), ������� �� �������� ��� ������ ������������� ��� �� ���������� ����
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Materials quantity for unloaded materials (AddMatsPreCount) = ',AddMatsPreCount
    RECL_ERR_FizZone2 = AddMatsCount !���������� �� ���-�� ����������, ������� �� ���������
    if (StepGlobal == 1) RECL_ERR_FizZone1 = RECL_ERR_FizZone2 !� ��������� ������ ������� ������ �� ���������
    !�� ���������� ����� ���������� (���������� ���), ������� �� ������ �������� (���������� ����� ����������)
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Added new fiz zones for recycling (AddMatsUniqueCount) = ',AddMatsUniqueCount
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'Added new fiz zones for previously unloaded &
    & materials (AddMatsPreUniqueCount) = ',AddMatsPreUniqueCount
    NewMatsCount = AddMatsUniqueCount + M1    + AddMatsPreUniqueCount
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 1) write(333, *) 'New value of materials quantity (AddMatsUniqueCount &
    &+ M1 + AddMatsPreUniqueCount) = ',NewMatsCount
    !������ ���������� ��������� ����������
    OldMatsCount = Nf
    if (isTEST >= 1) write(333, *) '--------'
    if (isTEST >= 2) write(333, *) 'Old value of fissile materials quantity = ',OldMatsCount
    !��������� ��� ���-�� ����������, ��� ��� ���������
    M1 = M1 + AddMatsUniqueCount    + AddMatsPreUniqueCount
    M1v = M1v + AddMatsUniqueCount     + AddMatsPreUniqueCount
    Naz = Naz + AddMatsUniqueCount   + AddMatsPreUniqueCount
    Nt = Nt + AddMatsUniqueCount   + AddMatsPreUniqueCount
    Nf = Nf + AddMatsUniqueCount   + AddMatsPreUniqueCount
    if (Ntor /= 0) Ntor = Ntor + AddMatsUniqueCount   + AddMatsPreUniqueCount
    if (Nbok /= 0) Nbok = Nbok + AddMatsUniqueCount   + AddMatsPreUniqueCount
    !��������� ������ ������� ����� ���
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
    i = Nf+1 !������� ������ ��� ����������� ����������
    do while (i > Nt)
      Ntnz(i)=Ntnz(i-AddMatsUniqueCount   - AddMatsPreUniqueCount)
      i = i + 1
    end do
    i = 1
    Ntnz = 4 !�� ��������� ������ ��� �������� (4 - ��� ����, ����������� � �������� ������)
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
    !��������� ������ �������������� � ������ �����
    i = OldMatsCount + 1 !� ������ ������� ������ ��� ��������� ���
    j = 1
    ! if (isTEST >= 2) write(333, *) 'Ntnz= ',Ntnz;
    !�����ר� ������ ������� ����� ������ �������� ����� �����ب�
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
