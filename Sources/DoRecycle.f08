!����������� ������� ����� ���������� ���� �� ��������� (RECYCLE)
subroutine DoRecycle
  use CommonModJARFR
  !����� ������ � �������
  character*250 :: curFileMassLoaded, curFileMassLoaded_TEMP, curFileMassUnloaded, curFileMassUnloaded_TEMP
  character*250 :: curFileMassLoaded_TEMP_BPSD, curFileMassUnloaded_BPSD, curFileMassUnloaded_TEMP_BPSD, curFileMassLoaded_BPSD
  character*250 :: curFileRecycledData
  !������ ��� �������� ����� � ������
  character*4 :: fmtStr
  !����� �������� ����
  integer :: NumZonIn, NumZonIn2
  !��� ���� (1 - ������ ����������, 2 - ���-�� ������, 3 - �������������� ���������)
  integer :: ZoneTypeIn
  !������ ��� ������ �����
  character*160 :: StrFile1, StrFile2
  !������ ��� ��������� ������
  character*6 AddedStr, AddEndOfOutputFile
  !�������� ����, �� ������� ���� ��������� ���� �������������� ���, ������� ������ ����������� � ��������
  integer StepUnload
  !������ ����, �� ������� ���� ��������� ���� �������������� ���, ������� ������ ����������� � ��������
  character*4 :: StepUnload_STR
  !�������� ����, �� ������� ���� ��������� �������������� ���, ������� ������ ����������� � ��������
  integer StepLoad
  !������ ����, �� ������� ���� ��������� �������������� ���, ������� ������ ����������� � ��������
  character*4 :: StepLoad_STR
  !������ ��� ������ �� �����
  character*6 :: Str6
  character*11 :: Str11
  character*15 :: str15
  character*16 :: str16
  character*150 :: Str150
  !���������� ��� ������ �����
  integer :: isot_id
  real :: isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
  real :: zone_volume
  !��������� ����� ����������� ����� ��� �����������
  real :: VolumeSumOut
  !�������������� ��������
  integer, allocatable :: isot_ids(:)
  !������ ��������� ����� ������� ������� �� ������ (In) � ����� ������ � �������� + �������� (Out)
  real, allocatable :: IsotMassSumIn(:), IsotMassSumOut(:)
  !������ ��������� ����� ������ �������� �� ������ (In) � ����� ������ � �������� + �������� (Out)
  real, allocatable :: GroupMassSumIn(:), GroupMassSumOut(:)
  !������ ����� ������ �������� ����� �����������
  real, allocatable :: GroupMassRecl(:)
  !������ ����� � ������������ ������� �� �������� ����� �����������
  real, allocatable :: IsotReclMass(:)
  !������ ������������� ��������� �������� ����� ������ � ����� (�����, ����� ����� ����� ����� ����������� ������ ��������)
  real, allocatable :: KRecl(:)
  !������� �����
  real atomic_mass
  !����������� �������� ������ (� JARFR �� � �3, � BPSD �� � ��3)
  real Kvol
  !������������ �������� �������
  character*4 IsotCapt, GetIsotopeCaption
  !���������� ������ ������ ���� ���
  integer is_first
  !Delta - ������� ����� ����������� ������ � ���������
  real delta_mass
  !�����
  real MassOfZeroGroupType !����� ��� ��������, ������� �� ������������
  real MassOf2ndGroupType  !����� ��� ��������, ������� �� ����� �������� �� �������������� �������
  real MassOf2ndGroupType_OLD  !������ ����� ��� ��������, ������� �� ����� �������� �� �������������� �������
  real MassSumOut          !����� ����������� ����� ���� ����������


  !����������� �������� ������ (� JARFR �� � �3, � BPSD �� � ��3)
  if (BpsdOn.EQ.1) then
    Kvol = 1
  else
    Kvol = 1E6
  end if


  !���, �� ������� ���� ��������� �����, ������� �� ������ ��������� ��������� ����� ����������
  StepUnload = StepGlobal - (nVRH+nRecl+1) !-1 ����� �������, ������ ��� �� ��� ��������� � StepGlobal ���� ���
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
  !����� ��������� ������ � �������������� �������, ����� �������� ����������� ����
  curFileRecycledData = 'recycle\'//trim(StepGlobal_STR)//'stp'//'\'//trim(StepGlobal_STR)//'_RecycledData.txt'
  if (isTEST >= 2) write(333, *) 'curFileRecycledData to recycle=',trim(curFileRecycledData)
  !��� ��������
  curFileMassUnLoaded_TEMP = 'recycle\'//trim(StepUnload_STR)//'stp'//'\'//trim(StepUnload_STR)//'_mass'
  curFileMassUnloaded_TEMP_BPSD = 'recycle_bpsd\'//trim(StepUnload_STR)//'stp'//'\'//trim(StepUnload_STR)//'_mass'
  AddedStr = AddEndOfOutputFile(4) !������ �� ������� ������
  curFileMassUnLoaded = trim(curFileMassUnLoaded_TEMP)//trim(AddedStr)//'.txt'
  curFileMassUnloaded_BPSD = trim(curFileMassUnloaded_TEMP_BPSD)//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'curFileMassUnLoaded to recycle=',trim(curFileMassUnLoaded)
  if (isTEST >= 2) write(333, *) 'curFileMassUnLoaded_BPSD to recycle=',trim(curFileMassUnloaded_BPSD)


  !�����������. ���� 1. ������� ��������� ��������� � �������� ����� ������� ������� �� ���� �����
  open (777,file=trim(curFileRecycledData),status='unknown')  !������ � �������������� �������
  VolumeSumOut = 0
  MassSumOut = 0 !����� ����������� ����� ���� ����������

  !�������� ������ ��� ������ �������� � �� ���� ��� ����������� ����������, � ����� ��� ����� ��������
  if (.not.allocated(isot_ids)) allocate(isot_ids(1:Ner1))
  isot_ids = isot_ids(1:Ner1)
  if (.not.allocated(IsotMassSumIn)) allocate(IsotMassSumIn(1:Ner1))
  if (.not.allocated(IsotMassSumOut)) allocate(IsotMassSumOut(1:Ner1))
  IsotMassSumIn = IsotMassSumIn(1:Ner1)
  IsotMassSumOut = IsotMassSumOut(1:Ner1)
  IsotMassSumIn = 0
  IsotMassSumOut = 0
  open (901,file=trim(curFileMassUnLoaded),status='unknown') !������ � ������� �� ������ �����������
10 read (901,'(a160)',end=15) StrFile2
  !���� �� ����� �� ����
  if (StrFile2(1:11).eq.'       ����') then
    read (StrFile2,'(a11,I5)') Str11, NumZonIn !      ����    1.
    if (isTEST >= 2) write(333, *) 'MassSum = > NumZonIn= ',NumZonIn
    read (901,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    if (isTEST >= 2) write(333, *) '  MassSum = > ZoneTypeIn= ',ZoneTypeIn
    read (901,'(a16,I4)') str16,StepLoad ! Loaded on Step     1
    !read (StrFile2,'(a15,I4)') str15, StepLoad
    if (isTEST >= 2) write(333, *) '  StepLoad= ',StepLoad
    read (901,*)  !     ----------
    read (901,*)  !                       NC            NF            ���          �����         �����-C       �����-F
    !������������ ������ �� �����, ������� ���� ��������� � ������������
    if (ZoneTypeIn.EQ.2) then
      !���, �� ������� ���� � ������ ��� ��������� ����� � ��, ������� �� ������ ��������� ��������� ����� � ����������
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
      !��� ��������� ���������� ����������, ������� ���, �� ������� ������ ��� ���� ������ ��������� (1-� ���)
      curFileMassLoaded_TEMP = 'recycle\'//trim(StepLoad_STR)//'stp'//'\'//trim(StepLoad_STR)//'_mass'
      curFileMassLoaded_TEMP_BPSD = 'recycle_bpsd\'//trim(StepLoad_STR)//'stp'//'\'//trim(StepLoad_STR)//'_mass'
      AddedStr = AddEndOfOutputFile(1)
      curFileMassLoaded = trim(curFileMassLoaded_TEMP)//trim(AddedStr)//'.txt'
      if (isTEST >= 2) write(333, *) 'curFileMassLOADED to recycle=',trim(curFileMassLoaded)
      open (900,file=trim(curFileMassLoaded),status='unknown')  !������ � ����������� �������
      !���� ������, ���������� ����� ������ ����
      ios = 0
      !iostat:
        ! = -1 error: end of file
        ! = -2 error: end of record
      findStrWithZone: do while (ios /= -1)
        read(900, '(a160)', advance = "yes", iostat = ios) StrFile1
        if (ios /= -1) then
          if (StrFile1(1:11).eq.'       ����') then
            read (StrFile1,'(a11,I5)') Str11, NumZonIn2 !      ����    1.
            if (NumZonIn2 == NumZonIn) then !���� �� ����� ���� ����
              if (isTEST >= 2) write(333, *) '  NumZonIn2= ',NumZonIn2
              !���������� ������� ���� � ������������ � �������� � ������ �����
              read (900,*)! TYPE:            1
              read (900,*)!  Loaded on Step    1
              read (900,*)  !     ----------
              read (900,*)  !                       NC            NF            ���          �����         �����-C       �����-F
              exit findStrWithZone
            end if
          end if
        end if
      end do findStrWithZone
      do i=1,Ner1
        !read (900,'(a160)') StrFile1
        !if (isTEST >= 2) write (*,*) StrFile1
        !��������� ������ ����������� � �������� ����� �� JARFR
        read (900,'(a15, I3, 6e14.5)') str15, isot_id, isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
        !if (isTEST >= 2) write (*,*) 'isot_id=',isot_id
        !read (900,'(a15, I3)') str15, isot_id
        isot_ids(i) = isot_id
        !���� �������� BPSD, �� �� ��������� ����� �������� Pu � U - ��������� �� ������
        if (BpsdOn.EQ.1) then
          if (BpsdModeIsots(i).NE.1) then !��� ��������� ������� �����
            IsotMassSumIn(i) = IsotMassSumIn(i) + isot_mass
          end if
        else
          IsotMassSumIn(i) = IsotMassSumIn(i) + isot_mass
        end if
        !��������� ������ ����������� � �������� ����� �� JARFR
        read (901,'(a15, I3, 6e14.5)') str15, isot_id, isot_nc, isot_nf, isot_mass, isot_alfa, isot_sigma_c, isot_sigma_f
        if (BpsdOn.EQ.1) then
          if (BpsdModeIsots(i).NE.1) then !��� ��������� ������� �����
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
      read (900,*)  ! ���������� ������   ����� � ����
      read (900,*)  ! ���������� ������   ------------------------------------------------------
      read (900,*)  ! ���������� ������   BEC U-235
      read (900,*)  ! ���������� ������   ��� U-238
      read (900,*)  ! ���������� ������   ��� PU
      read (900,*)  ! ���������� ������   ��� PU(9+1)
      !������ ������ "��ڨ� ����"
      read (900,'(a16,1e16.5)') Str16, zone_volume
      VolumeSumOut = VolumeSumOut + zone_volume  !������� ��������� �����, ���������� ����� ���������
      RECL_nzon_type(NumZonIn)=5    !������ ����� ������������ �������������� ����� ����
      write (333,*) 'Zone ',NumZonIn,' is empty after recycling. We can use it!'
      !RECL_Fiz_Enabled(NumZonIn)=0
      !rewind 900 !��������� � ������ �����, �� �������� ���� �������� ������ �� ���� �����
      close(900)
    end if
    !re_i = system("pause")
    read (901,*)  ! ���������� ������   ����� � ����
    read (901,*)  ! ���������� ������   ------------------------------------------------------
    read (901,*)  ! ���������� ������   BEC U-235
    read (901,*)  ! ���������� ������   ��� U-238
    read (901,*)  ! ���������� ������   ��� PU
    read (901,*)  ! ���������� ������   ��� PU(9+1)
    read (901,'(a16,1e16.5)') Str16, zone_volume
    read (901,*)
    read (901,*)
    read (901,*)
    goto 10
  else
    goto 10
  end if
15  close(901)


  !�� BPSD �������� ������ ������ ��� �������� Pu � U � �� ������
  if (BpsdOn.EQ.1) then
  open (901,file=trim(curFileMassUnloaded_BPSD),status='unknown') !������ � ������� �� ������ �����������
11 read (901,'(a160)',end=16) StrFile2
  !���� �� ����� �� ����
  if (StrFile2(1:11).eq.'       ����') then
    read (StrFile2,'(a11,I5)') Str11, NumZonIn !      ����    1.
    if (isTEST >= 2) write(333, *) 'MassSum = > NumZonIn= ',NumZonIn
    read (901,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    if (isTEST >= 2) write(333, *) '  MassSum = > ZoneTypeIn= ',ZoneTypeIn
    read (901,'(a16,I4)') str16,StepLoad ! Loaded on Step     1
    !read (StrFile2,'(a15,I4)') str15, StepLoad
    if (isTEST >= 2) write(333, *) '  StepLoad= ',StepLoad
    !������������ ������ �� �����, ������� ���� ��������� � ������������
    if (ZoneTypeIn.EQ.2) then
      !���, �� ������� ���� � ������ ��� ��������� ����� � ��, ������� �� ������ ��������� ��������� ����� � ����������
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
      !��� ��������� ���������� ����������, ������� ���, �� ������� ������ ��� ���� ������ ��������� (1-� ���)
      curFileMassLoaded_TEMP_BPSD = 'recycle_bpsd\'//trim(StepLoad_STR)//'stp'//'\'//trim(StepLoad_STR)//'_mass'
      AddedStr = AddEndOfOutputFile(1)
      curFileMassLoaded_BPSD = trim(curFileMassLoaded_TEMP_BPSD)//trim(AddedStr)//'.txt'
      !if (isTEST >= 2) write(333, *) 'curFileMassLoaded_BPSD to recycle=',trim(curFileMassLoaded_BPSD)
      open (900,file=trim(curFileMassLoaded_BPSD),status='unknown')  !������ � ����������� �������
      !���� ������, ���������� ����� ������ ����
      ios = 0
      !iostat:
        ! = -1 error: end of file
        ! = -2 error: end of record
      findStrWithZone2: do while (ios /= -1)
        read(900, '(a160)', advance = "yes", iostat = ios) StrFile1
        if (ios /= -1) then
          if (StrFile1(1:11).eq.'       ����') then
            read (StrFile1,'(a11,I5)') Str11, NumZonIn2 !      ����    1.
            if (NumZonIn2 == NumZonIn) then !���� �� ����� ���� ����
              if (isTEST >= 2) write(333, *) '  NumZonIn2= ',NumZonIn2
              !���������� ������� ���� � ������������ � �������� � ������ �����
              read (900,*)! TYPE:            1
              read (900,*)!  Loaded on Step    1
              exit findStrWithZone2
            end if
          end if
        end if
      end do findStrWithZone2
      do i=1,Ner1
        !��������� ������ ����������� � �������� ����� �� BPSD
        read (900,'(i3.3, e14.5)') isot_id, isot_mass
        isot_ids(i) = isot_id
        if (BpsdModeIsots(i).EQ.1) then !��� ��������� ������� �����
          IsotMassSumIn(i) = IsotMassSumIn(i) + isot_mass
        end if
        read (901,'(i3.3, e14.5)') isot_id, isot_mass
        if (BpsdModeIsots(i).EQ.1) then !��� ��������� ������� �����
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
      VolumeSumOut = VolumeSumOut + zone_volume  !������� ��������� �����, ���������� ����� ���������
      RECL_nzon_type(NumZonIn)=5    !������ ����� ������������ �������������� ����� ����
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


  !�����������. ���� 2. ������� ����� ������ ������ � �������������� � GroupRecl
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
      if (GroupRecl(j) == i) then !���� ���� ������ ���������� � ������ i, �� ��������� ��� ����� � ����� ������
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



  !�����������. ���� 3. ��������������� �������������� ������ ������ �������� � ������������ �� ��������� ����������
  ! �������� ��� ���������� ������ ������ ��������: 0 - �� ������� ������, �.�. ��� ����� ����� ��������, �� � ��������;
  !                                                 1 - ������� ������ ����� �����������;
  !                                                 2 - �������� ����� ������� �� �������������� �������, �.�. ��������/�������, ����� ����� ����� ��� ��, ��� � ����
  if (.not.allocated(GroupMassRecl)) allocate(GroupMassRecl(1:MaxGroupRecl))
  if (.not.allocated(KRecl)) allocate(KRecl(1:MaxGroupRecl))
  KRecl = KRecl(1:MaxGroupRecl)
  GroupMassRecl = GroupMassRecl(1:MaxGroupRecl)
  write (777,*) '==========================='
  write (777,*) 'K OF RECYCLING. Deltas (after minus before) of masses of groups [kg]'
  write (777,*) '------------------'
  write (777,*) ' ID  TYPE   K        MASS BEFORE   MASS AFTER    DELTA'
  n = 0
  MassOfZeroGroupType = 0 !����� ��� ��������, ������� �� ������������
  MassOf2ndGroupType_OLD = 0 !������ ����� ��� ��������, ������� �� ����� �������� �� �������������� �������
  !������� ����� ���, ������� �� �������
  do i=1,MaxGroupRecl
    n = TypeRecl(i)
    !0 - �� ������� ����� ���� �������� (���������� ���������)
    if (n == 0) then
      !������������ ��������� ����� �������� 0�� ���� ����������� ����� �����������
      MassOfZeroGroupType = MassOfZeroGroupType + GroupMassSumOut(i)*Kclear(i)
    end if
    !2 - �������� ����� ������� �� �������������� �������, �.�. ��������/�������, ����� ����� ����� ��� ��, ��� � ����
    if (n == 2) then
      !������ ����� ��� ��������, ������� �� ����� �������� �� �������������� ������� ����� �����������
      MassOf2ndGroupType_OLD = MassOf2ndGroupType_OLD + GroupMassSumOut(i)*Kclear(i)
    end if
  end do
  !����� �����, ������� ����� �������� �� ������ ������, ����� ����� ��� ����� ��������������
  MassOf2ndGroupType = MassSumOut - MassOfZeroGroupType - MassOf2ndGroupType_OLD
  do i=1,MaxGroupRecl
    n = TypeRecl(i)
    !0 - �� ������� ����� ���� �������� (���������� ���������)
    if (n == 0) then
      GroupMassRecl(i) = GroupMassSumOut(i)*Kclear(i)
    end if
    !NOT RECCOMENDED TO USE THIS !!!! !1 - ������� ������ ����� ����������� (��������� ���� �� Kclear)
    if (n == 1) GroupMassRecl(i) = 0
    !2 - �������� ����� ������� �� �������������� �������, �.�. ��������/�������, ����� ����� ����� ��� ��, ��� � ����
    if (n == 2) then
!      GroupMassRecl(i) = MassOf2ndGroupType*GroupMassSumIn(i)/MassOf2ndGroupType_OLD  !����� ������� ������� - ��� ����� ����� �������� �� ���� ����� ������� ����������
       GroupMassRecl(i) = GroupMassSumOut(i)*Kclear(i)
    end if
    if (GroupMassSumOut(i) > 0) then
      KRecl(i) = GroupMassRecl(i)/GroupMassSumOut(i)     !���� ��������� ������. ��������� ��� ������ ������ ��������
    else
      KRecl(i) = 0 !���� ����� ���, �� � ���� ���)) - ����� ������ ��� ������������� BPSD
    end if
    delta_mass = GroupMassRecl(i)-GroupMassSumIn(i)    !������� ���� ���������, ������� ����� �� ���� ����
    write (777,'(i4,i4,f11.4,3e14.5)') i, n, KRecl(i), GroupMassRecl(i), GroupMassSumOut(i), delta_mass
  end do
  write(333, *) 'Mass of constant materials for TypeRecl=0, MassOfZeroGroupType = ',MassOfZeroGroupType
  write(333, *) 'Mass of unloaded materials for TypeRecl=2, MassOf2ndGroupType_OLD = ',MassOf2ndGroupType_OLD
  write(333, *) 'Mass that to be added for fullfil Assembly to it previous mass '// &
  &'(for TypeRecl=2), MassOf2ndGroupType = ',MassOf2ndGroupType
  write (777,'(//)')



  !�����������. ���� 4. �������, ������� ����� ������������ ������� ������� ����� ����������� (��� ���������� ��� ���� ���)
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
    !����, � ����� ������ ������������ ������
    n = GroupRecl(i)
    !�������� ������������ ������� �� ������ ������ (������������ �� ������ �� ���� �3 � ������������ �� ������� ������)
    !��������� ����������� ����� ��� TypeRecl=2 � ������������ � ������ �������� � ����� ����� ���� ���������
    IsotReclMass(i) = KRecl(n)*IsotMassSumOut(i) + GrReclFraq(i)*MassOf2ndGroupType
    !�������� ������������� �������� �������
    isot_id = isot_ids(i)
    !������� ������� ����� �������� �������
    atomic_mass = GetIsotopeAtomicMass(isot_id)
    !������� ������������ �� ����� �������� �������
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
  close(777)  !��������� ���� � ������� �� �����������



  !����������� ��������� ������� ��� ��������� ������ ����� ������
  deallocate(isot_ids)
  deallocate(IsotMassSumIn)
  deallocate(IsotMassSumOut)
  deallocate(GroupMassSumIn)
  deallocate(GroupMassSumOut)
  deallocate(IsotReclMass)
  deallocate(GroupMassRecl)
  deallocate(KRecl)
end subroutine DoRecycle
