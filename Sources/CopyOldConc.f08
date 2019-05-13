!��������� �������� ������������ ��� ������ ���� (�������� ����������, ���� ����� ����������� ������� �����)
subroutine CopyOldConc
  use CommonModJARFR
  !����� ������ � ��������������
  character*250 :: curFile_CONC, curFile_CONC_TEMP
  !������ ��� �������� ����� � ������
  character*4 :: fmtStr
  !���������� ������ AD - ������������ ������� �� ������� ��������
  real, allocatable :: ADNEWx2(:,:)
  !����� �������� ����
  integer :: NumZonIn
  !��� ���� (1 - ������ ����������, 2 - ���-�� ������, 3 - �������������� ���������)
  integer :: ZoneTypeIn
  !���������� ������������� �������
  integer :: IsotID
  !��������� ������������
  real ConcIn
  !������ ��� ������ �����
  character*160 StrFile1
  !������ ��� ��������� ������
  character*6 AddedStr, AddEndOfOutputFile
  !�������� ����������� ����
  integer StepGlobalMinusOne
  character*4 :: StepGlobalMinusOne_STR
  !������ ��� ������ �� �����
  character*11 :: Str11
  character*6 :: Str6
  character*150 :: Str150
  !����� ����������� ������ � ����������� ����
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
  !��������� ������� �������� AD
  !  �����: 1. ������ ����������� ��� �������, ������� ����������� � ����������� �� ��������� ���� �� �����
  !         2.�. �������� ����������� �� �������� (���� Step < N�����������), �� ����� ���� �������� �� ������, ������ ��� ���� �� ���������� ����
  !         2.�. �������� ����������� �� �������������� (���� Step >= N�����������)
  !������ ��������� ������ �� AD, ����� ����� � ��� ���� ����������
  k = 1
  allocate(ADNEWx2(1:M1, 1:Ner1))
  do i=1,M1
    do j=1,Ner1
      ADNEWx2(i,j) = Ad(k)
      k = k + 1
    end do
  end do

  !��������� � AD ����� ������������ ��� ����������� ���� �� ����������� ����. ������� - ��������� �������
  curFile_CONC_TEMP = 'recycle\'//trim(StepGlobalMinusOne_STR)//'stp'//'\'//trim(StepGlobalMinusOne_STR)//'_conc' !������������
  AddedStr = AddEndOfOutputFile(3) !������� ��� ���������� ����� �� ������ �������� �� �������� + ��������� �������� (3-� ���)
  curFile_CONC = trim(curFile_CONC_TEMP)//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'curFile_CONC to copy for Fissile nuclides=',trim(curFile_CONC)
  open (5,file=trim(curFile_CONC),status='unknown')
10  read (5,'(a150)', end=15) StrFile1
  !���� �� ����� �� ����
  if (StrFile1(1:11).eq.'       ����') then
    read (StrFile1,'(a11,I5)') Str11, NumZonIn !      ����    1.
    if (isTEST >= 2) write(333, *) 'NumZonIn= ',NumZonIn
    read (5,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    read (5,*)   !'Loaded on Step ', StepGlobal
    !�������� ������ �� ������������, ������� �������������� �� ����� (�.�. ��������� ����. � �� - ���� ���� 1 � 3)
    if (isTEST >= 2) write(333, *) 'ZoneTypeIn= ',ZoneTypeIn

    do i=1,Ner1 !��� ������� ���������� ������� ������ ��� ������
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
  !��������� � AD ����� ������������ ��� ����������� ���� �� ����������� ����. ������ - PU � U �� BPSD
  !����� ������ �� ������������, ������������ �� ������� ���� � ������� BPSD
  curFile_CONC_TEMP = 'recycle_bpsd\'//trim(StepGlobalMinusOne_STR)//'stp'//'\'//trim(StepGlobalMinusOne_STR)//'_conc' !������������
  AddedStr = AddEndOfOutputFile(3) !������� ��� ���������� ����� �� ������ �������� �� �������� + ��������� �������� (3-� ���)
  curFile_CONC = trim(curFile_CONC_TEMP)//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'curFile_CONC to copy for KM=',trim(curFile_CONC)
  open (5,file=trim(curFile_CONC),status='unknown')
11  read (5,'(a150)', end=16) StrFile1
  !���� �� ����� �� ����
  if (StrFile1(1:11).eq.'       ����') then
    read (StrFile1,'(a11,I5)') Str11, NumZonIn !      ����    1.
    if (isTEST >= 2) write(333, *) 'NumZonIn= ',NumZonIn
    read (5,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    read (5,*)   !'Loaded on Step ', StepGlobal
    !�������� ������ �� ������������, ������� �������������� �� ����� (�.�. ��������� ����. � �� - ���� ���� 1 � 3)
    if (isTEST >= 2) write(333, *) 'ZoneTypeIn= ',ZoneTypeIn

    do i=1,Ner1 !��� ������� ������� ������ ��� ������
      read (5,'(i3.3, e14.5)') IsotID, ConcIn
      !���������� ��������� - ��� � ��� ��� ����
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



  !����� ��������� ��������� � �������� ���������� ������
  n = 1
  Ad=Ad(1:(M1*Ner1))
  do i=1,M1
    do j=1,Ner1
      Ad(n) = ADNEWx2(i,j)
      n = n + 1
    end do
  end do

  !����������� ��������� ��������� ������ ����� �������
  deallocate(ADNEWx2)
end subroutine CopyOldConc
