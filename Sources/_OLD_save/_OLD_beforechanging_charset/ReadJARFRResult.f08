!������������ �������� ����� JARFR
subroutine ReadJARFRResult(InpFileName)
    use CommonModJARFR
    character*180 :: InpFileName
    write(333, *) '  SAVE RESULTS TO FILES - balance.txt';
    call SaveAllInfo(InpFileName)
    write(333, *) '  SAVE RESULTS TO FILES - balance.txt - DONE!';
end subroutine



!������� �������� ������ �� ������� ������� � ��������� ����� � ��������� ����� �� �����
subroutine SaveAllInfo(InpFileNameIn)
    use CommonModJARFR
    character*180 :: InpFileNameIn
    integer :: NumZonIn !������ �������� ����, ��� ������� ����� ����� ������
    character*250 :: curFile, curFile_TEMP, curFile_CONC, curFile_CONC_TEMP, curFile_KR, &
     &  curFile_KV
    character*250 :: str2
    character*10 :: Str10
    character*15 :: str15
    character*16 :: str16, StrFileDotted
    character*25 :: str25
    integer, allocatable :: isot_ids(:)
    real, allocatable :: conc_in(:)
    real atomic_mass
    real krv !����������� ��������������� �� V
    real KVcore, KVreactor !����������� ��������������� �������� ���� � ��������

    real MassPu239and240, MassPuAll, MassU235, MassU238 !�������� ���� Pu, U �� ���� ��������
    real zero1,zero2,zero3


    !���������� ��� ��������� ����������� ��������
    integer :: isot_id;
    real :: isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
    real :: zone_volume

    integer :: count_num, is_first, in_counter
    character*160 :: StrFile1, StrFile2 !������ ��� ������ �����
    character*6 AddedStr, AddEndOfOutputFile

    !��������������/���������� ����������
    count_num = 1
    in_counter = 0
    is_first=1

    if (isTEST >= 2) write(333, *) 'Reading data about isotopes...'
    curFile_KR = trim(CurDir)//'\'//trim(StepGlobal_STR)//'_kr.txt' !Kr - ����������� ���������������
    curFile_KV = trim(CurDir)//'\'//trim(StepGlobal_STR)//'_KV.txt' !�� - ����������� ���������������


    curFile = trim(CurDir)//'\'//trim(StepGlobal_STR)//'_mass'
    curFile_CONC = trim(CurDir)//'\'//trim(StepGlobal_STR)//'_conc' !������������
    AddedStr = AddEndOfOutputFile(count_num)
    curFile_TEMP = trim(curFile)//trim(AddedStr)//'.txt'
    curFile_CONC_TEMP = trim(curFile_CONC)//trim(AddedStr)//'.txt'
    open (999,file=curFile_TEMP,status='unknown') !���������� ����� ��������������� ������������, ���� �������������� �������
      write (999, *) '-----------------------------------'
      write (999, *) 'MASSES OF ISOTOPES FOR ALL OF ZONES'
      write (999, *) 'GLOBAL STEP = ',StepGlobal_STR
      write (999, *) 'INSIDE TIME (IT) = ',count_num
      write (999, *) '  IT = 1 => BEGIN OF STEP'
      write (999, *) '  IT = 2 => END OF STEP'
      write (999, *) '  IT = 3 => END OF STEP + COOLING'
      write (999, *) '  IT = 4 => AFTER RECYCLING'
    open (998,file=curFile_CONC_TEMP,status='unknown') !���������� ����� ��������������� ������������, ���� �������������� �������
    open (997,file=curFile_KR,status='unknown') !����������� ��������������� Kr
      write (997, *) 'Kr - ����������� ��������������� �� V �� �����'
    open (996,file=curFile_KV,status='unknown') !����������� ��������������� KV
      write (996, *) '�� - ����������� ��������������� �������� ���� (��) � �������� (�-�) �� �����, NC/NCF'
      write (996, *) '   i      KVCore   KVreactor'

    !if (isTEST >= 2) write(333, *) 'curFile_CONC_TEMP='//trim(curFile_CONC_TEMP)

    open (5,file=trim(InpFileNameIn),status='unknown')

    !�������� ������ ��� ������ �������� � �� ������������ ��� ����������� ����������
    if (.not.allocated(conc_in)) allocate(conc_in(1:Ner1));
    if (.not.allocated(isot_ids)) allocate(isot_ids(1:Ner1));
    conc_in = conc_in(1:Ner1);
    isot_ids = isot_ids(1:Ner1);
    RECL_ERR_volume2 = 0

10  read (5,'(a160)', end=15) StrFile1
    if (StrFile1(1:26).eq.'                ����� ����') then      !����� ������� ����, � ������� ����� ���������� ������, ������ ��� ��� ��� ��������� ���
      is_first = 0
      in_counter = in_counter + 1
      !write(*,*) 'Finding after burnup, in_counter',in_counter
      if (in_counter < NDTnum) then
        goto 10 !���������� ������ ��� ����� �� ������ ������ ��������, ��� ����� ��������� NDTnum ��� ������ ��������
      else
      end if
      close(999)
      count_num = count_num + 1 !���� ��������� ��������� ������ (�� ����� 1-�� (330 �����) ���� ������ ��������, ����� - �� ����� 30 �����, ����� - �� ������ �������� � ��������� ��� � ������� �����������)
      AddedStr = AddEndOfOutputFile(count_num)
      curFile_TEMP = trim(curFile)//trim(AddedStr)//'.txt'
      open (999,file=curFile_TEMP,status='unknown') !���������� �����, ��������������� ������������, ���� �������������� �������
      curFile_CONC_TEMP = trim(curFile_CONC)//trim(AddedStr)//'.txt'
      open (998,file=curFile_CONC_TEMP,status='unknown') !���������� ������������, ��������������� ������������, ���� �������������� �������

      write (999, *) '-----------------------------------';
      write (999, *) 'MASSES OF ISOTOPES FOR ALL OF ZONES';
      write (999, *) 'GLOBAL STEP = ',StepGlobal;
      write (999, *) 'INSIDE TIME (IT) = ',count_num;
      write (999, *) '  IT = 1 => BEGIN OF STEP';
      write (999, *) '  IT = 2 => END OF STEP';
      write (999, *) '  IT = 3 => END OF STEP + COOLING';
      write (999, *) '  IT = 4 => AFTER RECYCLING';
    end if


    !��������� ����� ����������
    if (StrFile1(1:57).eq.'           �������� ��������� �������� �� ����� ��������') then
      if (is_first == 0) then
        if (in_counter < NDTnum) then
          goto 10 !���������� ������ ��� ����� �� ������ ������ ��������, ��� ����� ��������� NDTnum ��� ������ ��������
        end if
      end if
      write(999,*) '==========================================================================='
      write(999,'(a160)') StrFile1
      read (5,'(a160)') StrFile2 !
      write(999,'(a160)') StrFile2
      read (5,'(a160)') StrFile2 !                         ���.����      ���.�����     ���.�����      �������
      write(999,'(a160)') StrFile2
      read (5,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU235
      write(999,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU235
      read (5,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU238
      write(999,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU238
      read (5,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPuAll
      write(999,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPuAll
      read (5,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPu239and240
      write(999,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPu239and240
     ! write (999,'(i5 f15.4, f15.5, 4f14.4)') StepNumberIn, DaysBegin, KeffBegin, MassPuAll, MassPu239and240, MassU235, MassU238
      write(999,*) '==========================================================================='
      write(999,*)
      write(999,*)
      write(999,*)
      goto 10
    end if


    !��������� Kr - ����������� ��������������� �� ������ �������� ����
    if (StrFile1(1:31).eq.'   KO���.HEPABHOMEPHOCT� �� *V*') then
      read (StrFile1,'(a63,f12.5)') str2, krv !f12.5 - ������������ �����, ������� �������� � ����� 12 ��������, � ����� ����� ���� 5 �������� ����
      write(997,'(i5, f12.5)') in_counter, krv
    end if


    !��������� �� - ����������� ��������������� �������� ���� (��) � �������� (�-�)
    if (StrFile1(1:40).eq.'   �����.��������������� ( NC/NCF.��  )=') then
      read (StrFile1,'(a40,f12.5)') str2, KVcore !f12.5 - ������������ �����, ������� �������� � ����� 12 ��������, � ����� ����� ���� 5 �������� ����
    end if
    if (StrFile1(1:40).eq.'                         ( NC/NCF.P-P )=') then
      read (StrFile1,'(a40,f12.5)') str2, KVreactor
      write(996,'(i5, 2f12.5)') in_counter, KVcore, KVreactor
    end if




    !���� �� ����� �� ������ ��� ����
    if (StrFile1(1:10).eq.'      ����') then
      read (5,'(a160)') StrFileDotted  ! ��������� ������      ----------
      if (StrFileDotted(1:16).NE.'      ----------') then  !���� ���������� ������ �� ��������, �� ��� ����� ������ � ��� ��� �� �����
        goto 10
      end if
      if (is_first == 0) then
        if (in_counter < NDTnum) then
          goto 10 !���������� ������ ��� ����� �� ������ ������ ��������, ��� ����� ��������� NDTnum ��� ������ ��������
        end if
      end if
      !if (isTEST >= 2) write(333, *) 'Finding ZONE: '//trim(StrFile1)
      !if (isTEST >= 2) write(333, *) 'is_first=',is_first;
      read (StrFile1,'(a10,I5)') Str10, NumZonIn
      n = NumZonIn
      !if (isTEST >= 2) write(333, *) 'Finding ZONE ',NumZonIn,' - OK'
      write(999,*) '======================================='
      write(998,*) '======================================='
      !if (isTEST >= 2) write(333, *) 'curFile=',trim(curFile)
      write(999,*) StrFile1
      write(998,*) StrFile1

      str2 = '.';
      if (count_num == 4) then
        if (isTEST >= 2) write(333, *) 'BEFORE RECL_nzon_type(',n,')=',RECL_nzon_type(n)
      end if
      if (RECL_nzon_type(n) == 2) then
        str2 = ' - ZONE TO BE RECYCLED ON THIS STEP!!!'  !������ ��� ���������� ������ ���� �����������
!        if (StepGlobal == 1) write (StrFile2,'(a15,I4)') 'Loaded on Step ', StepGlobal !�� ������ ���� ��-����� ��� �����
      end if
      if (RECL_nzon_type(n) == 3) then
        str2 = ' - Constructive materials'  !��������������� ���������
      end if
      if (RECL_nzon_type(n) == 5) then
        str2 = ' - Constructive materials'  !������������ ����� (���������� ����� �����������)
      end if
      if (RECL_nzon_type(n) == 0) then
        str2 = ' - Restricted to use (is in VRH now)'  !������������ ������ (� ������ ������ ��������� �� ���������������� ���������)
      end if
      if (RECL_nzon_type(n) == 4) then
        str2 = ' - NEW ZONE'  !4-� ��� ��������, ��� ��� - ����� �������� (���� ����� ���, ���� �����������)
        !if (isTEST >= 2) write(333, *) trim(StrFile2)
        !������ ����������������� ������ � �����
        !if (count_num == 4) then
        !  if (n <= Nf) then
        !    RECL_nzon_type(n) = 1
        !  else
        !    RECL_nzon_type(n) = 3
        !  end if
        !end if
      end if
      if (count_num == 4) then
        if (isTEST >= 2) write(333, *) 'AFTER RECL_nzon_type(',n,')=',RECL_nzon_type(n)
      end if
      write (StrFile2,'(a15,I4)') 'Loaded on Step ', RECL_FizStepLoaded(n)
      write(999,'(a6,I4,a150)') 'TYPE: ',RECL_nzon_type(n),str2
      write(998,'(a6,I4,a150)') 'TYPE: ',RECL_nzon_type(n),str2
      write(999,*) StrFile2  !'Loaded on Step ', StepGlobal
      write(998,*) StrFile2
      write(999,'(a16)') StrFileDotted
      read (5,'(a160)') StrFile2  ! ���������� ������                        NC            NF            ���          �����         �����-C       �����-F
      write(999,'(a160)') StrFile2
      do i=1,Ner1 !��� ������� ������� ������ ��� ������
        read (5,'(a15, i3.3, 6e14.5)') str15, isot_id, isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
        write (999,'(a15, i3.3, 6e14.5)') str15, isot_id, isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
        atomic_mass = GetIsotopeAtomicMass(isot_id)
        conc_in(i) = isot_mass*CONST_NA/(0.001*1E6*CONST_barn*atomic_mass) !������� ������������ ��� ����������� �������������
        isot_ids(i) = isot_id
        !��������� ��� �������� ����������� ����������� ����� (��� �� ������ ������ ��������)
        if ((isot_id == 235) .OR. (isot_id == 238) .OR. (isot_id == 236) .OR. (isot_id == 234)) then
          if (count_num == 1) then
            !����������� ����� �� ���� ����
            if (RECL_FizStepLoaded(NumZonIn) == StepGlobal) then
              if (RECL_nzon_type(NumZonIn) == 1) then
                MassDataU(StepGlobal)%mass_loaded = MassDataU(StepGlobal)%mass_loaded + isot_mass
                if (isot_id == 234) then
                  UVector_LOADED(StepGlobal)%U234 = UVector_LOADED(StepGlobal)%U234 + isot_mass
                end if
                if (isot_id == 235) then
                  UVector_LOADED(StepGlobal)%U235 = UVector_LOADED(StepGlobal)%U235 + isot_mass
                end if
                if (isot_id == 236) then
                  UVector_LOADED(StepGlobal)%U236 = UVector_LOADED(StepGlobal)%U236 + isot_mass
                end if
                if (isot_id == 238) then
                  UVector_LOADED(StepGlobal)%U238 = UVector_LOADED(StepGlobal)%U238 + isot_mass
                end if
              end if
            end if
            !����� ����� � �� � ������ ��������
            MassDataU(StepGlobal)%mass_before = MassDataU(StepGlobal)%mass_before + isot_mass
          end if
          if (count_num == 3) then
            !����� ����� � �� � ����� ��������
            MassDataU(StepGlobal)%mass_after = MassDataU(StepGlobal)%mass_after + isot_mass
            if (isot_id == 234) UVector(StepGlobal)%U234 = UVector(StepGlobal)%U234 + isot_mass
            if (isot_id == 235) UVector(StepGlobal)%U235 = UVector(StepGlobal)%U235 + isot_mass
            if (isot_id == 236) UVector(StepGlobal)%U236 = UVector(StepGlobal)%U236 + isot_mass
            if (isot_id == 238) UVector(StepGlobal)%U238 = UVector(StepGlobal)%U238 + isot_mass
            !����������� ����� �� ���� ���� ��� �����������
            if (RECL_nzon_type(NumZonIn) == 2) then
              MassDataU(StepGlobal)%mass_unloaded = MassDataU(StepGlobal)%mass_unloaded + isot_mass
            end if
          end if
        end if
!        if ((isot_id == 239) .OR. (isot_id == 240) .OR. (isot_id == 241) .OR. (isot_id == 242)) then
        if ((isot_id == 239) .OR. (isot_id == 240) .OR. (isot_id == 241) .OR. (isot_id == 242) &
        & .OR. (isot_id == 248) .OR. (isot_id == 246)) then
          if (count_num == 1) then
            !����������� ����� �� ���� ����
            if (RECL_FizStepLoaded(NumZonIn) == StepGlobal) then
!              if ((RECL_nzon_type(NumZonIn) == 1).OR.(RECL_nzon_type(NumZonIn) == 2)) then
              if (RECL_nzon_type(NumZonIn) == 1) then
                MassDataPu(StepGlobal)%mass_loaded = MassDataPu(StepGlobal)%mass_loaded + isot_mass
                if (isot_id == 246) then
                  PuVector_LOADED(StepGlobal)%Pu236 = PuVector_LOADED(StepGlobal)%Pu236 + isot_mass
                end if
                if (isot_id == 248) then
                  PuVector_LOADED(StepGlobal)%Pu238 = PuVector_LOADED(StepGlobal)%Pu238 + isot_mass
                end if
                if (isot_id == 239) then
                  PuVector_LOADED(StepGlobal)%Pu239 = PuVector_LOADED(StepGlobal)%Pu239 + isot_mass
                end if
                if (isot_id == 240) then
                  PuVector_LOADED(StepGlobal)%Pu240 = PuVector_LOADED(StepGlobal)%Pu240 + isot_mass
                end if
                if (isot_id == 241) then
                  PuVector_LOADED(StepGlobal)%Pu241 = PuVector_LOADED(StepGlobal)%Pu241 + isot_mass
                end if
                if (isot_id == 242) then
                  PuVector_LOADED(StepGlobal)%Pu242 = PuVector_LOADED(StepGlobal)%Pu242 + isot_mass
                end if
!               write (333, *) '���� �',NumZonIn,' -> ',isot_id,'=',isot_mass
              end if
            end if
            !����� ����� � �� � ������ ��������
            MassDataPu(StepGlobal)%mass_before = MassDataPu(StepGlobal)%mass_before + isot_mass
          end if
          if (count_num == 3) then
            !����� ����� � �� � ����� ��������
            MassDataPu(StepGlobal)%mass_after = MassDataPu(StepGlobal)%mass_after + isot_mass
            if (isot_id == 246) PuVector(StepGlobal)%Pu236 = PuVector(StepGlobal)%Pu236 + isot_mass
            if (isot_id == 248) PuVector(StepGlobal)%Pu238 = PuVector(StepGlobal)%Pu238 + isot_mass
            if (isot_id == 239) PuVector(StepGlobal)%Pu239 = PuVector(StepGlobal)%Pu239 + isot_mass
            if (isot_id == 240) PuVector(StepGlobal)%Pu240 = PuVector(StepGlobal)%Pu240 + isot_mass
            if (isot_id == 241) PuVector(StepGlobal)%Pu241 = PuVector(StepGlobal)%Pu241 + isot_mass
            if (isot_id == 242) PuVector(StepGlobal)%Pu242 = PuVector(StepGlobal)%Pu242 + isot_mass
            !����������� ����� �� ���� ���� ��� �����������
            if (RECL_nzon_type(NumZonIn) == 2) then
              MassDataPu(StepGlobal)%mass_unloaded = MassDataPu(StepGlobal)%mass_unloaded + isot_mass
            end if
          end if
        end if
      end do
      read (5,'(a160)') StrFile2  ! ���������� ������   ����� � ����
      write(999,'(a160)') StrFile2
      read (5,'(a160)') StrFile2  ! ���������� ������   ------------------------------------------------------
      write(999,'(a160)') StrFile2
      read (5,'(a160)') StrFile2  ! ���������� ������   BEC U-235
      write(999,'(a160)') StrFile2
      read (5,'(a160)') StrFile2  ! ���������� ������   ��� U-238
      write(999,'(a160)') StrFile2
      read (5,'(a160)') StrFile2  ! ���������� ������   ��� PU
      write(999,'(a160)') StrFile2
      read (5,'(a160)') StrFile2  ! ���������� ������   ��� PU(9+1)
      write(999,'(a160)') StrFile2
      !������ ������ "��ڨ� ����"
      read (5,'(a16,1e16.5)') Str16, zone_volume
      write(999,'(a16,1e16.5)') Str16, zone_volume
      do i=1,Ner1
        if (zone_volume.ne.0) then
          conc_in(i) = conc_in(i)/zone_volume !������ ��� �������� ����� � �� ����� �������� ������������� �������� ������������ � 1/(��*����)
        end if
        write (998,'(i3.3, e14.5)') isot_ids(i), conc_in(i)
      end do
      !��������� ������: ���� ��� ������� ��������� ����������� �����
      if (count_num == 1) then
        if (RECL_nzon_type(n) == 2) then
          RECL_ERR_volume2 = RECL_ERR_volume2 + zone_volume
        end if
      end if
      write(999,*)
      write(999,*)
      write(999,*)
      write(998,*)
      write(998,*)
      write(998,*)
      goto 10
    else
      goto 10
    end if
15  close(5)
    close(999)
    close(998)
    deallocate(isot_ids)
    deallocate(conc_in)
    close(997);
    close(996);

    if (StepGlobal == 1) RECL_ERR_volume1 = RECL_ERR_volume2
    if (isTEST >= 2) write(333, *) 'RECL_ERR_volume2=',RECL_ERR_volume2
end subroutine

