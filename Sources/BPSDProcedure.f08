!����������� ��������� ������ ��������� BPSD
subroutine BPSDProcedure
  use CommonModJARFR

  !������, ���������� ���� �� ����� � ���������� � ��� ��������
  character*180 :: BPSDexeFileName, BPSDPath, TempFileNameInput,  TempFileNameOutputBPSD,  &
    & TempFileNameOutputAllConc, TempFileNameOutputAllMass
  !������ ��� ��������� ������
  character*6 AddedStr, AddEndOfOutputFile
  !���� �� �����, ���������� ������� ������������ ��� BPSD
  character*180 :: BPSDinputConcFileName
  !���� �� ��������� ����� JARFR, ����������� ������
  character*180 :: BPSDJARFRresFileName
  !���� �� ��������� ����� CONSYST, ����������� �������
  character*180 :: BPSDCONSYSTresFileName
  !���� �� ��������� ����� WEIGHTS, ����������� ������
  character*180 :: BPSDweightsFileName

  !��� ������ �� �������� �� BPSD (������� ������������ ������� ID �� JARFR)
  type TNuclDataBPSD
    character*4     nucl_caption           !�������� ������� ��� ��� ������� �� ������� �����
    character*4     nucl_caption_JARFR     !�������� ������� ��� ��� ������� � JARFR/CONSYST
    character*6     nucl_out_caption_BPSD  !�������� ������� ��� ��� ������� � �������� ����� BPSD
    integer         nucl_numberID          !����� ������� �� ���� JARFR
    real            conc_temp              !��������� �������� ������������ ��� ������� ������ ���� BPSD
    real            deviation_temp         !��������� �������� ���������� ��� ������� ������ ���� BPSD
  end type
  type(TNuclDataBPSD), allocatable :: NuclDataBPSD(:)

  !������� ������� � ������� ��� ������� �������� �� BPSD ��� ������ ������
!  real :: SigmaF(1:27, 1:26), SigmaC(1:27, 1:26), SigmaN2N(1:27, 1:26), SigmaN3N(1:27, 1:26)
  real, allocatable :: SigmaF(:, :, :), SigmaC(:, :, :), SigmaN2N(:, :, :), SigmaN3N(:, :, :)
  real, allocatable :: FluxAbs(:, :), FluxFraq(:, :), FluxAbsFull(:)
  real :: FluxIntegralTEMP, VolumeTEMP
  real :: SigmasReadTEMP(1:6)
  !real, allocatable :: SigmasReadTEMP2(:)
  !��������� ����� ������
  real RandomDeltaOfDeviation


  character*20         :: fmtStr                  !������ ��� �������� ����� � ������
  character*4          :: fmtStr2                 !������ ��� �������� ����� � ������
  character*4          :: fmtStr3                 !������ ��� �������� ����� � ������
  integer              :: NumZonIn                !����� �������� ����
  integer              :: ZoneTypeIn              !��� ���� (1 - ������ ����������, 2 - ���-�� ������, 3 - �������������� ���������)
  integer              :: IsotID                  !���������� ������������� �������
  real                 :: ConcIn                  !��������� ������������
  real                 :: MassIn                  !��������� �����
  integer, allocatable :: ConcInCaption(:)        !������������� ������� �� ������� ����� JARFR
  character*160        :: StrFile1, StrFile2      !������ ��� ������ �����
  character*4          :: NuclJARFRname           !������ ��� ����������� �������� �������
  character*11         :: Str11                   !������ ��� ������ �� �����
  character*6          :: Str6
  character*150        :: Str150
  character*9999       :: Str9999
  integer              :: ZoneConsystNumIn        !����� ����������� ���� ��������
  character*4          :: NumZoneIn_Str           !����� ����������� ���� �������� - ������
  character*4          :: Nf_STR                  !���-�� ��������� ��� �� ������� ����� JARFR - ������
  character*4          :: l_Str                   !����� ���� �� ��������� BPSD - ������
  real, allocatable    :: time_in(:), dtime_in(:) !!��������� ���� � ������� �������� �� ������ ���������
  real, allocatable    :: ZonesVol(:)             !�������� ������� ���
  real                 :: atomic_mass             !������� �����
  integer              :: isot_id                 !������������� ������� �� JARFR ��� ��������� ��� �����

  real, allocatable    :: ConcInArr(:), ConcInArrBPSD(:), DeviationArrBPSD(:), MassInArrBPSD(:) !�������� ������������

  !������ �������� ���� BPSD
  NAMELIST /IND/ nmfiles
  NAMELIST /INA/ time,dtime,tstepb,tstepp,initer,epp,epb,nstep
  integer :: nstep, initer
  real :: tstepb, tstepp, epp, epb, time, dtime !!time, dtime - ��������� ���� � ������� �������� �� ������ ���������
  character *99 :: nmfiles !������ �������� �����

  allocate(NuclDataBPSD(1:27))
  !������ ������������ ����� ��������� �� BPSD � JARFR
  NuclDataBPSD(1)%nucl_caption           = 'u235'
  NuclDataBPSD(1)%nucl_numberID          = 235
  NuclDataBPSD(1)%nucl_out_caption_BPSD  = 'U235  '
  NuclDataBPSD(2)%nucl_caption           = 'u236'
  NuclDataBPSD(2)%nucl_numberID          = 236
  NuclDataBPSD(2)%nucl_out_caption_BPSD  = 'U236  '
  NuclDataBPSD(3)%nucl_caption           = 'u238'
  NuclDataBPSD(3)%nucl_numberID          = 238
  NuclDataBPSD(3)%nucl_out_caption_BPSD  = 'U238  '
  NuclDataBPSD(4)%nucl_caption           = 'pu39'
  NuclDataBPSD(4)%nucl_numberID          = 239
  NuclDataBPSD(4)%nucl_out_caption_BPSD  = 'Pu239 '
  NuclDataBPSD(5)%nucl_caption           = 'pu40'
  NuclDataBPSD(5)%nucl_numberID          = 240
  NuclDataBPSD(5)%nucl_out_caption_BPSD  = 'Pu240 '
  NuclDataBPSD(6)%nucl_caption           = 'pu41'
  NuclDataBPSD(6)%nucl_numberID          = 241
  NuclDataBPSD(6)%nucl_out_caption_BPSD  = 'Pu241 '
  NuclDataBPSD(7)%nucl_caption           = 'pu42'
  NuclDataBPSD(7)%nucl_numberID          = 242
  NuclDataBPSD(7)%nucl_out_caption_BPSD  = 'Pu242 '
  NuclDataBPSD(8)%nucl_caption           = 'fp35'
  NuclDataBPSD(8)%nucl_numberID          = 215
  NuclDataBPSD(8)%nucl_out_caption_BPSD  = 'POISON'
  NuclDataBPSD(9)%nucl_caption           = 'Th30'
  NuclDataBPSD(9)%nucl_numberID          = 232
  NuclDataBPSD(9)%nucl_out_caption_BPSD  = 'Th230 '
  NuclDataBPSD(10)%nucl_caption          = 'Pa31'
  NuclDataBPSD(10)%nucl_numberID         = 211
  NuclDataBPSD(10)%nucl_out_caption_BPSD = 'Pa231 '
  NuclDataBPSD(11)%nucl_caption          = 'U232'
  NuclDataBPSD(11)%nucl_numberID         = 0
  NuclDataBPSD(11)%nucl_out_caption_BPSD = 'U232  '
  NuclDataBPSD(12)%nucl_caption          = 'U233'
  NuclDataBPSD(12)%nucl_numberID         = 233
  NuclDataBPSD(12)%nucl_out_caption_BPSD = 'U233  '
  NuclDataBPSD(13)%nucl_caption          = 'U234'
  NuclDataBPSD(13)%nucl_numberID         = 234
  NuclDataBPSD(13)%nucl_out_caption_BPSD = 'U234  '
  NuclDataBPSD(14)%nucl_caption          = 'Np37'
  NuclDataBPSD(14)%nucl_numberID         = 227
  NuclDataBPSD(14)%nucl_out_caption_BPSD = 'Np237 '
  NuclDataBPSD(15)%nucl_caption          = 'Np39'
  NuclDataBPSD(15)%nucl_numberID         = 229
  NuclDataBPSD(15)%nucl_out_caption_BPSD = 'Np239 '
  NuclDataBPSD(16)%nucl_caption          = 'Pu36'
  NuclDataBPSD(16)%nucl_numberID         = 246
  NuclDataBPSD(16)%nucl_out_caption_BPSD = 'Pu236 '
  NuclDataBPSD(17)%nucl_caption          = 'Pu38'
  NuclDataBPSD(17)%nucl_numberID         = 248
  NuclDataBPSD(17)%nucl_out_caption_BPSD = 'Pu238 '
  NuclDataBPSD(18)%nucl_caption          = 'Am41'
  NuclDataBPSD(18)%nucl_numberID         = 201
  NuclDataBPSD(18)%nucl_out_caption_BPSD = 'Am241 '
  NuclDataBPSD(19)%nucl_caption          = 'Am2m'
  NuclDataBPSD(19)%nucl_numberID         = 202
  NuclDataBPSD(19)%nucl_out_caption_BPSD = 'Am242m'
  NuclDataBPSD(20)%nucl_caption          = 'Am43'
  NuclDataBPSD(20)%nucl_numberID         = 203
  NuclDataBPSD(20)%nucl_out_caption_BPSD = 'Am243 '
  NuclDataBPSD(21)%nucl_caption          = 'Cm42'
  NuclDataBPSD(21)%nucl_numberID         = 222
  NuclDataBPSD(21)%nucl_out_caption_BPSD = 'Cm242 '
  NuclDataBPSD(22)%nucl_caption          = 'Cm43'
  NuclDataBPSD(22)%nucl_numberID         = 223
  NuclDataBPSD(22)%nucl_out_caption_BPSD = 'Cm243 '
  NuclDataBPSD(23)%nucl_caption          = 'Cm44'
  NuclDataBPSD(23)%nucl_numberID         = 224
  NuclDataBPSD(23)%nucl_out_caption_BPSD = 'Cm244 '
  NuclDataBPSD(24)%nucl_caption          = 'U237'
  NuclDataBPSD(24)%nucl_numberID         = 0
  NuclDataBPSD(24)%nucl_out_caption_BPSD = 'U237  '
  NuclDataBPSD(25)%nucl_caption          = 'NP36'
  NuclDataBPSD(25)%nucl_numberID         = 0
  NuclDataBPSD(25)%nucl_out_caption_BPSD = 'Np236 '
  NuclDataBPSD(26)%nucl_caption          = 'NP38'
  NuclDataBPSD(26)%nucl_numberID         = 0
  NuclDataBPSD(26)%nucl_out_caption_BPSD = 'Np238 '
  NuclDataBPSD(27)%nucl_caption          = 'A42G'
  NuclDataBPSD(27)%nucl_numberID         = 0
  NuclDataBPSD(27)%nucl_out_caption_BPSD = 'Am242g'

  !������� ������������ ������������� �� JARFR/CONSYST �������
  do i=1,27
    do j=1,99
      if (NuclDataBPSD(i)%nucl_numberID == NuclDataCommon(j)%nucl_numberID) then
        NuclDataBPSD(i)%nucl_caption_JARFR = NuclDataCommon(j)%nucl_Caption
      end if
    end do
  end do

  !������ ���� �� exe-����� BPSD � �� �����
  open(unt1, file = 'settings\bpsdexefile.txt', status = 'OLD')
  read(unt1,'(A)') BPSDexeFileName
  read(unt1,'(A)') BPSDPath
  if (isTEST >= 3) write(333, *) 'BPSDexeFileName=',trim(BPSDexeFileName)
  if (isTEST >= 3) write(333, *) 'BPSDPath=',trim(BPSDPath)
  close(unt1)

  allocate(time_in(1:NDTnum+2))
  allocate(dtime_in(1:NDTnum+2))

  !�������������� ���������� ��� ������ �� ������� ����
  nmfiles = 'BPSD_Input_conc.txt'
!     *TIME/30.0,5.0,90.0,30.0,120.0,30.0,4*0.0/,  !��������� �������, �� ������� �� �������
!     *dtime/1.0,0.0,1.0,0.0,1.0,0.0,4*0.0/,       !������� �������� �� ������ ���������
  do i=1,NDTnum !��������� ������ �������� �������
    time_in(i) = TCamp/NDTnum  !����� ������ �� �������� (330 ���/���-�� ����������)
    if (i <= NDTnum) then
      dtime_in(i) = 1  !�������� - ��������� (���������� �� 1)
    end if
  end do
  time_in(NDTnum + 1) = TDown  !����� �������� (30 ���)
  time_in(NDTnum + 2) = (nVRH+nRecl)*T1Recl  !����� �������� + ����������� (2+1 ���) �� ���������������� ���������
  !�������� �� �����
  dtime_in(NDTnum + 1) = 0.00001  !�������� �������� (30 ���)
  dtime_in(NDTnum + 2) = 0.00001  !�������� �������� + ����������� (2+1 ���) �� ���������������� ���������
!    *nstep/6/,                                   !���������� ����� �� ���������
!  nstep = NDTnum + 2
  nstep = 1
!  *tstepb/1000.0/,                             !���-�� ���������� ��� ���ר�� ��������� ����������
!     *tstepp/10000.0/,                            !���-�� ���������� ��� ���ר�� ��������� ��
  tstepb = 100000.0   !��� ������ time_in, ��� ������� ����� ���� ���� ����� (��� 730 ����� �������� 10 �����)
  tstepp = 1000000.0  !��� ������ time_in, ��� ������� ����� ���� ���� ����� (��� 730 ����� �������� 100 �����)
!  *initer/0/,                                  !����� �������� �� ����������� ����������� �������� �� ��������� ����, ���� initer=0, �� �������� ���������� �� ���������� �������� �������� epb � epp
!     *epp/0.01/,                                  !��������, ����� ������� ������������ ������� ������� �����������
!     *epb/0.01/                                   !��������, ����� ������� ������������ ������� ������� �����������
  initer = 0
  epp = 0.001
  epb = 0.001

  !������� ������ �������� ��� ������ � BPSD - ����� ����� ��������� ��������� �����
  command = 'mkdir recycle_bpsd\  1>>tmp 2>&1'
  if (isTEST >= 2) write(333, *) command
  re_i = system(command)
  command = 'mkdir recycle_bpsd\'//trim(StepGlobal_STR)//'stp\  1>>tmp 2>&1'
  if (isTEST >= 2) write(333, *) command
  re_i = system(command)

  !�������� ����, ���������� ��������� ������������ ���� �������� ��� ���� ���
  AddedStr = AddEndOfOutputFile(1) !������� ��� ���������� ����� �� ������ �������� �� �������� + ��������� �������� (3-� ���)
  BPSDinputConcFileName = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_conc'//trim(AddedStr)//'.txt'
  BPSDJARFRresFileName = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_results.txt'
  BPSDCONSYSTresFileName = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_consyst.txt'
  BPSDweightsFileName = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_weights.txt'
  command = 'copy recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_conc'//trim(AddedStr)//'.txt &
    & '//trim(BPSDinputConcFileName)//' 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  !�������� �������� ���� JARFR, ���������� ������
  command = 'copy recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_results.txt &
    & '//trim(BPSDJARFRresFileName)//' 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  !�������� �������� ���� CONSYST, ���������� �������
  command = 'copy recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_consyst.txt &
    & '//trim(BPSDCONSYSTresFileName)//' 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  !�������� �������� ���� JARFR - WEIGHTS, ���������� ������
  command = 'copy recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_weights.txt &
    & '//trim(BPSDweightsFileName)//' 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)

  !������ ���
  allocate(ZonesVol(1:Nf))

  !�������� ������������ � ����� ������� �������
  allocate(ConcInArrBPSD(1:Ner1))
  allocate(MassInArrBPSD(1:Ner1))
  allocate(ConcInCaption(1:Ner1))
  allocate(DeviationArrBPSD(1:Ner1))
  ConcInArrBPSD = 0
  MassInArrBPSD = 0
  ConcInCaption = 0
  DeviationArrBPSD = 0

  !��������� ������� �� ��������� ����� CONSYST
  !������������� ����������
  allocate(SigmaF(1:Nf, 1:27, 1:26))
  allocate(SigmaC(1:Nf, 1:27, 1:26))
  allocate(SigmaN3N(1:Nf, 1:27, 1:26))
  SigmaF = 0
  SigmaC = 0
  SigmaN3N = 0
  open (78,file=trim(BPSDCONSYSTresFileName),status='unknown')
  if (isTEST >= 2) write(333, *) 'BPSDCONSYSTresFileName to run=',trim(BPSDCONSYSTresFileName)
12 read (78,'(a150)', end=17) StrFile1
  !����� ������������������ ����� ��������� ��� ���� �
  !write(333,*) StrFile1(1:28)
  if (StrFile1(1:28).eq.'         Self-shielded MICRO') then
    read(StrFile1, '(a51,i3)') Str150,ZoneConsystNumIn
    if (isTEST >= 2) write(333, *) 'Found MicroConst for ZoneConsystNumIn = ',ZoneConsystNumIn
    if (ZoneConsystNumIn > Nf) goto 12 !���� �� ��������� ����, �� - �������
    !��� ������� �������
    do i=1,Ner1
      read (78,'(a18, a4)') StrFile2, NuclJARFRname  !     For isotope  U235
      !������� ����� ������� � ������������������ �������� �� BPSD
      n = 0
      do j=1,27
        if (NuclDataBPSD(j)%nucl_caption_JARFR == NuclJARFRname) then
          n = j
        end if
      end do
      read (78,*)
      do j=1,26
        read(78,'(i4,6e12.3)') m,(SigmasReadTEMP(k),k=1,6)
        if (n > 0) then
          SigmaC(ZoneConsystNumIn, n, j) = SigmasReadTEMP(2)
          SigmaF(ZoneConsystNumIn, n, j) = SigmasReadTEMP(4)
        end if
      end do
    end do
  end if
  goto 12
17 close(78)



  !��������� ������� N2N �� ��������� ����� JARFR
  !������������� ����������
  !��������� ����� ���� � ������
  SELECT CASE (Nf)
   CASE (1:9)
      fmtStr3 = '(I1)'
   CASE (10:99)
      fmtStr3 = '(I2)'
   CASE (100:999)
      fmtStr3 = '(I3)'
   CASE (1000:9999)
      fmtStr3 = '(I4)'
   CASE DEFAULT
      fmtStr3 = '(I4)'
  END SELECT
  write (unit=Nf_Str, FMT=fmtStr3) Nf !��������� ����� Nf � ������ Nf_STR
  !write(*,*) '(i4,'//trim(Nf_STR)//'e10.3)'

  allocate(SigmaN2N(1:Nf,1:27, 1:26))
  SigmaN2N = 0
  open (98,file=trim(BPSDJARFRresFileName),status='unknown')
  if (isTEST >= 2) write(333, *) 'BPSDJARFRresFileName to run=',trim(BPSDJARFRresFileName)
31 read (98,'(a9999)', end=35) Str9999
  !n2n �������������� ��� ������� X �� 26 ������� (������) �� ������ �� ��� (�������)
  if (Str9999(1:28).eq.'   sigN2') then
    !��� ������� �� ���������� ��������
    do i=1,Nred
      !read(Str9999, '(a36,i3)') Str150,ZoneConsystNumIn  !������ ����� �������
      read (98,'(a36, i3)') Str150, isot_id !   next isotope          15         223
      if (isTEST >= 2) write(333, *) 'Found N2N Sigmas for Isotope = ',isot_id
      !������� ����� ������� � ������������������ �������� �� BPSD
      n = 0
      do j=1,27
        if (NuclDataBPSD(j)%nucl_numberID == isot_id) then
          n = j
        end if
      end do
      do j=1,26
        if (n.ne.0) then
          read (98,*) l,(SigmaN2N(k,n,j),k=1,Nf)
          write(333,*) l,(SigmaN2N(k,n,j),k=1,Nf)
        else
          write(333,*) 'Isotope is not found in BPSD. Skipped.'
        end if
      end do


      !allocate(SigmasReadTEMP2(1:Nf))
      do j=1,26
      !  read(98,*) Str9999
      !  write(333,*) 'Nf_STR = (i7, '//trim(Nf_STR)//'e8.2)'
   !     read(Str9999,'(i7, '//trim(Nf_STR)//'e8.2)') m,(SigmasReadTEMP2(k),k=1,Nf)
        !write(*,*) Str9999(1:50)
        do k=1,Nf
          if (n > 0) then
            !SigmaN2N(k, n, j) = SigmasReadTEMP2(k)
            !write(*,*) SigmasReadTEMP2(k)
          end if
        end do
      end do
      !deallocate(SigmasReadTEMP2)
    end do
  end if
  goto 31
35 close(98)



  !��� ������� ���� ���������� ��������� BPSD
  l = 1
  do while (l <= NDTnum + 2)

    !write(*,*)
    !write(*,*) 'STEP #'//trim(StepGlobal_STR)
    write(*,'(a,i3,a4,i3)') 'STEP #'//trim(StepGlobal_STR)//'. BPSD INNER STEP #',l, ' OF ',(NDTnum + 2)

    !��������� ����� ���������� ���� � ������
    SELECT CASE (l)
     CASE (1:9)
        fmtStr3 = '(I1)'
     CASE (10:99)
        fmtStr3 = '(I2)'
     CASE (100:999)
        fmtStr3 = '(I3)'
     CASE (1000:9999)
        fmtStr3 = '(I4)'
     CASE DEFAULT
        fmtStr3 = '(I4)'
    END SELECT
    write (unit=l_Str, FMT=fmtStr3) l !��������� ����� l � ������ l_Str




    !��������� ��� ������ �������� ������������ ��� ������ ���� �� ������ ��������� �������
    TempFileNameOutputAllConc = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\' &
      & //trim(StepGlobal_STR)//'stp_conc_'//trim(l_Str)//'.txt'
    open (601,file=trim(TempFileNameOutputAllConc),status='unknown')
    if (isTEST >= 2) write(333, *) 'Output file for all zones TempFileNameOutputAllConc=',trim(TempFileNameOutputAllConc)
    !��������� ��� ������ �������� ���� ��� ������ ���� �� ������ ��������� �������
    TempFileNameOutputAllMass = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\' &
      & //trim(StepGlobal_STR)//'stp_mass_'//trim(l_Str)//'.txt'
    open (602,file=trim(TempFileNameOutputAllMass),status='unknown')
    if (isTEST >= 2) write(333, *) 'Output file for all zones TempFileNameOutputAllMass=',trim(TempFileNameOutputAllMass)

    !��������� ������ �� ��������� ����� JARFR
    allocate(FluxAbsFull(1:Nf))
    allocate(FluxAbs(1:Nf, 1:26)) !����� � Nf ���� 26 �����
    allocate(FluxFraq(1:Nf, 1:26)) !����� � Nf ���� 26 �����
    l_in = 0  !���������� ������� ��� ������ ��������� ���������� ������
    open (77,file=trim(BPSDJARFRresFileName),status='unknown')
11  read (77,'(a150)', end=16) StrFile1
    !����� ����� � ������ �� ���
    if (StrFile1(1:13).eq.'    VOLs,cm^3') then
      read (77,*)
      do j=1,Nf
        read (77, '(i6,3e12.5)') k, VolumeTEMP, FluxIntegralTEMP, FluxAbsFull(j)
        ZonesVol(j) = VolumeTEMP
      end do
    end if
    !������
    if (StrFile1(1:36).eq.'    NORMALIZED SPECTRUM IN EACH ZONE') then
      l_in = l_in + 1
      if (l_in.NE.l) goto 11 !���� ��� �� ����� �� ��������� ������ �� �������� �������� l, �� ���� ������
      if (isTEST >= 2) write(333,*) 'Founded Flux l_in = ',l_in
      read (77,*)
      read (77,*) !   0         1         2         3         4         5         6         7         8         9        10        11        12        13        14        15        16        17
      read (77,*)
      !write(*,*) '(i4,'//trim(Nf_STR)//'e10.3)'
      write(fmtStr, *) '(i4,'//trim(Nf_STR)//'e10.3)'
      !write(*,*) 'fmtStr = ',fmtStr
      do j=1,26
        read(77,fmtStr) k, (FluxFraq(i, j),i=1,Nf)
      end do
      !������ �� ����� ��������� ���������� ����� � ������ ������ �� ������ ������ ������ � ������ ����
      do i=1,Nf
        do j=1,26
          FluxAbs(i,j) = FluxAbsFull(i)*FluxFraq(i,j)
        end do
      end do
      goto 16
    end if
    goto 11
16  close(77)



    !��������� ������ �� ����� WEIGHTS.REZ - �� ������������ - �������
    open (87,file=trim(BPSDweightsFileName),status='unknown')
14  read (87,'(a150)', end=19) StrFile1
    !����� ����� � ������ �� ���
    !������
    if (StrFile1(1:14).eq.'   ZONE       ') then
      l_in = l_in + 1
      if (l_in.NE.l) goto 11 !���� ��� �� ����� �� ��������� ������ �� �������� �������� l, �� ���� ������
      if (isTEST >= 2) write(333,*) 'Founded Flux l_in = ',l_in
      read (77,*)
      read (77,*) !   0         1         2         3         4         5         6         7         8         9        10        11        12        13        14        15        16        17
      read (77,*)
      !��������� ����� ���� � ������
      SELECT CASE (Nf)
       CASE (1:9)
          fmtStr3 = '(I1)'
       CASE (10:99)
          fmtStr3 = '(I2)'
       CASE (100:999)
          fmtStr3 = '(I3)'
       CASE (1000:9999)
          fmtStr3 = '(I4)'
       CASE DEFAULT
          fmtStr3 = '(I4)'
      END SELECT
      write (unit=Nf_Str, FMT=fmtStr3) Nf !��������� ����� Nf � ������ Nf_STR
      !write(*,*) '(i4,'//trim(Nf_STR)//'e10.3)'
      write(fmtStr, *) '(i4,'//trim(Nf_STR)//'e10.3)'
      !write(*,*) 'fmtStr = ',fmtStr
      do j=1,26
       ! read(77,fmtStr) k, (FluxFraq(i, j),i=1,Nf)
      end do
      !������ �� ����� ��������� ���������� ����� � ������ ������ �� ������ ������ ������ � ������ ����
      do i=1,Nf
        do j=1,26
         ! FluxAbs(i,j) = FluxAbsFull(i)*FluxFraq(i,j)
        end do
      end do
      goto 16
    end if
    goto 14
19  close(87)


    !������ ��������� �������� ������������ ��� ������ �� ��� � ������� �� BPSD
    if (isTEST >= 2) write(333, *) 'BPSDinputConcFileName to run=',trim(BPSDinputConcFileName)
    open (5,file=trim(BPSDinputConcFileName),status='unknown')
10  read (5,'(a150)', end=15) StrFile1
    !���� �� ����� �� ����
    if (StrFile1(1:11).eq.'       ����') then
      read (StrFile1,'(a11,I5)') Str11, NumZonIn !      ����    1.
      if (isTEST >= 2) write(333, *) 'NumZonIn= ',NumZonIn
      read (5,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
      if (ZoneTypeIn == 3) then
        goto 10 !��������������� ��������� ����������
      end if

      !���������� �������� ���� ��� �������� ���� � ������ ����
      write(601,*) '======================================='
      write(602,*) '======================================='
      write(601,'(a150)') StrFile1 !       ����    1.
      write(602,'(a150)') StrFile1 !       ����    1.
      write(601,'(a6,I4)') Str6,ZoneTypeIn !TYPE:            1
      write(602,'(a6,I4)') Str6,ZoneTypeIn !TYPE:            1
      read (5,'(a150)') StrFile1  !'Loaded on Step ', StepGlobal
      write(601, '(a150)') StrFile1
      write(602, '(a150)') StrFile1
      if (isTEST >= 2) write(333, *) 'ZoneTypeIn= ',ZoneTypeIn

      !��������� �������� ������������
      allocate(ConcInArr(1:27))
      ConcInArr = 0
      do i=1,Ner1 !��� ������� ������� ������ ��� ������������
        read (5,'(i3.3, e14.5)') IsotID, ConcIn
        !��������� �������� ������������ � ����� ��������
        ConcInArrBPSD(i) = ConcIn
        ConcInCaption(i) = IsotID
        do j=1,27
          if (NuclDataBPSD(j)%nucl_numberID == IsotID) ConcInArr(j) = ConcIn !���������� � ������ ��� ������� �� BPSD
        end do
        !if (isTEST >= 2) write(333, *) 'Not doing anything'
      end do

      !��������� ����� ���� � ������
      SELECT CASE (NumZonIn)
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
      write (unit=NumZoneIn_Str, FMT=fmtStr2) NumZonIn !��������� ����� NumZonIn � ������ NumZonIn_STR

      write(*, *) '     BPSD is running for zone #...'//trim(NumZoneIn_Str);

      !��������� ������� ���� BPSD ��� ������ ��� ������� ����, ��� ������ ������
      TempFileNameInput = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_'// &
        & trim(l_Str)//'_'//trim(NumZoneIn_Str)//'_inp.txt'
      open (88,file=trim(TempFileNameInput),status='unknown')
      ! Concentration
      write(88,*) 'Concentration'
      do i=1,27
       write(88,'(i3,x,a4,x,1pe12.4)') i,NuclDataBPSD(i)%nucl_caption,ConcInArr(i)
      end do
      ! Capture cross-section
      write(88,*) 'Capture cross-section'
      write(88,*) '                   1           2           3           4           5           6  &
      &         7           8           9          10          11          12          13          14     &
      &     15          16          17          18          19          20          21          22        &
      &  23          24          25          26'
      do i=1,27
       write(88,'(i3,x,a4,x,1p26e12.4)') i,NuclDataBPSD(i)%nucl_caption,(SigmaC(NumZonIn, i, j),j=1,26)
      end do
      !  Fission cross-section
      write(88,*) 'Fission cross-section'
      write(88,*) '                   1           2           3           4           5           6  &
      &         7           8           9          10          11          12          13          14     &
      &     15          16          17          18          19          20          21          22        &
      &  23          24          25          26'
      do i=1,27
       write(88,'(i3,x,a4,x,1p26e12.4)')i,NuclDataBPSD(i)%nucl_caption,(SigmaF(NumZonIn, i, j),j=1,26)
      end do
      !  (n,2n) cross-section
      write(88,*) '(n,2n) cross-section'
      write(88,*) '                   1           2           3           4           5           6  &
      &         7           8           9          10          11          12          13          14     &
      &     15          16          17          18          19          20          21          22        &
      &  23          24          25          26'
      do i=1,27
       write(88,'(i3,x,a4,x,1p26e12.4)')i,NuclDataBPSD(i)%nucl_caption,(SigmaN2N(NumZonIn, i, j),j=1,26)
      end do
      !  (n,3n) cross-section
      write(88,*) '(n,3n) cross-section'
      write(88,*) '                   1           2           3           4           5           6  &
      &         7           8           9          10          11          12          13          14     &
      &     15          16          17          18          19          20          21          22        &
      &  23          24          25          26'
      do i=1,27
       write(88,'(i3,x,a4,x,1p26e12.4)')i,NuclDataBPSD(i)%nucl_caption,(SigmaN3N(NumZonIn, i, j),j=1,26)
      end do
      !   Neutron flux  spectrum flux
      write(88,*) '   Neutron flux  spectrum flux'
      do j=1,26
        write(88,'(i3,x,1p2e12.4)')j,FluxAbs(NumZonIn,j),FluxFraq(NumZonIn,j)
      end do
      close(88)
      deallocate(ConcInArr)

      !������ �������� ����� ��� ������ �� ��� � ��������� �� ��-������� � BPSD
      command = 'copy '//trim(TempFileNameInput)//' &
        & '//trim(BPSDPath)//trim(nmfiles)//' 1>>tmp 2>&1'  !D:\JARFR\REPRORYV\bpsd_exe\BPSD_Input_conc.txt
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      !������� ��������� ������� ���� �� ����� REPRORYV (����� �� ������� �����)
      command = trim(DelComm)//' "'//trim(TempFileNameInput)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)


      !���������� ����� ������� ���� � ����������� ���ר�� ��� ������� ���� � ����� � BPSD
      time = time_in(l) !��������� ������� �������� ������� � �������������� ������ �������� ��� ������ �� ������� ���� BPSD
      dtime = dtime_in(l)
      open(222, file = trim(BPSDPath)//'BPSD_INPUT.txt', status = 'unknown')
      write(222, NML = IND)
      write(222, NML = INA)
      close(222)


      !��������� BPSD
      if (isTEST >= 2) write(333,*) 'call "'//trim(BPSDexeFileName)//'" 1>bpsdlog.txt 2>bpsderr.txt'
      !write(*,*) 'RUNNING BPSD FOR BURNUP (CELL #'//trim(NumZoneIn_Str)//', INNER_STEP=#'//trim(l_Str)//')...'
      re_i = system('call "'//trim(BPSDexeFileName)//'" 1>bpsdlog.txt 2>bpsderr.txt')
      !write(*,*) 'RUNNING BPSD FOR BURNUP... DONE'
      !������������ � ������� �����
      if (isTEST >= 2) write(333,*) 'cd /d %~dp0 1>>tmp 2>&1'
      re_i = system('cd /d %~dp0 1>>tmp 2>&1')

      !�������� ��������� ������ BPSD ������� � ���� �����
      TempFileNameOutputBPSD = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_' &
        & //trim(l_Str)//'_'//trim(NumZoneIn_Str)//'_out.txt'
      command = 'copy '//trim(BPSDPath)//'BPSD_OUTPUT.txt &
        & '//trim(TempFileNameOutputBPSD)//' 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)

      !������������ �������� ���� BPSD ��� ���������� ������������ ����� ������� ���������� ���� �� �������
      open (78,file=trim(TempFileNameOutputBPSD),status='unknown')
      if (isTEST >= 2) write(333, *) 'TempFileNameOutputBPSD to collect=',trim(TempFileNameOutputBPSD)
13    read (78,'(a150)', end=18) StrFile1
      !������������ ���������� � ������� �������
      if (StrFile1(1:24).eq.' Actinides concentration') then
        read(78, *) ! time intervals 1
        StrFile1 = ''
        CopyConc: do
          read(78, '(a150)') StrFile1
          if (StrFile1(1:4).EQ.' ---') then
            exit CopyConc
          end if
          !write(*,*) StrFile1
          read(StrFile1, '(a6,1p8G10.3)') AddedStr, ConcIn
          do i=1,27
            if (AddedStr == NuclDataBPSD(i)%nucl_out_caption_BPSD) then
              NuclDataBPSD(i)%conc_temp = ConcIn
              write(333,*) 'NuclDataBPSD(j)%conc_temp1=',NuclDataBPSD(i)%conc_temp
            end if
          end do
        end do CopyConc
      end if
      !���������� ���������� � ������� �������
      if (StrFile1(1:37).eq.' Deviation of actinides concentration') then
        read(78, *) ! time intervals 1
        StrFile1 = ''
        CopyConc2: do
          read(78, '(a150)') StrFile1
          if (StrFile1(1:4).EQ.' ---') then
            exit CopyConc2
          end if
          read(StrFile1, '(a6,1p8G10.3)') AddedStr, ConcIn
          do i=1,27
            if (AddedStr == NuclDataBPSD(i)%nucl_out_caption_BPSD) then
              NuclDataBPSD(i)%deviation_temp = ConcIn
            end if
          end do
        end do CopyConc2
      end if
      goto 13
18    close(78)

      !������� ��������� �������� ���� �� ����� REPRORYV (����� �� ������� �����)
      command = trim(DelComm)//' "'//trim(TempFileNameOutputBPSD)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)

      !��������� �������� ������������ � ����� � ����� �����
      do i=1,Ner1
        ConcInArrBPSD = 0
        !�������� ������������� �������� �������
        isot_id = ConcInCaption(i)

        do j=1,27
          if (NuclDataBPSD(j)%nucl_numberID == ConcInCaption(i)) then
            ConcInArrBPSD(i) = NuclDataBPSD(j)%conc_temp
            DeviationArrBPSD(i) = NuclDataBPSD(j)%deviation_temp
          end if
        end do

        !���� ����� ������� ������� ������ ������������ (���������� ������) - ������ ��� PU � U
        if (BpsdMode>=1) then
          call random_number(RandomDeltaOfDeviation)
        end if
        if (BpsdMode == 1) then
          if (BpsdModeIsots(i).EQ.1) then !��� ��������� ����� ��� ������� �������
            !write (*,*) 'ConcInArrBPSD(i) WAS = ',ConcInArrBPSD(i)
            ConcInArrBPSD(i) = ConcInArrBPSD(i) + ConcInArrBPSD(i)*DeviationArrBPSD(i)*RandomDeltaOfDeviation
            !write (*,*) 'ConcInArrBPSD(i) BECOME = ',ConcInArrBPSD(i)
          end if
        !���� ����� ������� ������ ������ ������������ (�������� ������) - ������ ��� PU � U
        else if (BpsdMode == 2) then
          if (BpsdModeIsots(i).EQ.1) then !��� ������� ����� ��� ������� �������
            ConcInArrBPSD(i) = ConcInArrBPSD(i) - ConcInArrBPSD(i)*DeviationArrBPSD(i)*RandomDeltaOfDeviation
          end if
        end if

        write(601,'(i3.3, 2e14.5)') ConcInCaption(i), ConcInArrBPSD(i), DeviationArrBPSD(i)
        !��� ��������� ������� ������������ �����
        if (ZonesVol(NumZonIn) > 0 ) then
          !������� ������� ����� �������� �������
          atomic_mass = GetIsotopeAtomicMass(isot_id)
          !������� ������������ �� ����� �������� �������
          MassInArrBPSD(i) = ConcInArrBPSD(i)/CONST_NA*(0.001*CONST_barn*atomic_mass*ZonesVol(NumZonIn))
          write(602,'(i3.3, 2e14.5)') ConcInCaption(i), MassInArrBPSD(i), DeviationArrBPSD(i)
        end if
      end do
      write(601, *)
      write(601, *)
      write(601, *)
      write (602,'(a4, e14.5)') 'VOL:',ZonesVol(NumZonIn)
      write(602, *)
      write(602, *)

      read(5,*)  !��� ������ ������ ����� ������������
      read(5,*)
      read(5,*)
      goto 10
    else
      goto 10
    end if
15  close(5)

    deallocate(FluxAbs)
    deallocate(FluxFraq)
    deallocate(FluxAbsFull)
    !��������� ���� ������������ � ���� ��� ���� ��� TempFileNameOutputAllConc � TempFileNameOutputAllMass
    close(601)
    close(602)

    !��������� ��� �� ��������� - ����� ������������ ������������ �� ������ ������� ���� TempFileNameOutputAllConc
    BPSDinputConcFileName = TempFileNameOutputAllConc
    l = l + 1
  end do



  !��������������� ��������� ����� � ������ REPRORYV � ������� ������ �����
  l = 1
  do while (l <= NDTnum + 2)

    !��������� ����� ���������� ���� � ������
    SELECT CASE (l)
     CASE (1:9)
        fmtStr3 = '(I1)'
     CASE (10:99)
        fmtStr3 = '(I2)'
     CASE (100:999)
        fmtStr3 = '(I3)'
     CASE (1000:9999)
        fmtStr3 = '(I4)'
     CASE DEFAULT
        fmtStr3 = '(I4)'
    END SELECT
    write (unit=l_Str, FMT=fmtStr3) l !��������� ����� l � ������ l_Str
    TempFileNameOutputAllConc = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\' &
      & //trim(StepGlobal_STR)//'stp_conc_'//trim(l_Str)//'.txt'
    !��������� ��� ������ �������� ���� ��� ������ ���� �� ������ ��������� �������
    TempFileNameOutputAllMass = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\' &
      & //trim(StepGlobal_STR)//'stp_mass_'//trim(l_Str)//'.txt'

    if (l < NDTnum) then !���������� �������� ��� ������ �������� - ���, � �����-�� �� �����
      !������� ��������� �������� ���� �� ����� REPRORYV (����� �� ������� �����)
      if (isTEST >= 2) write(333, *) 'Delete conc file TempFileNameOutputAllConc=',trim(TempFileNameOutputAllConc)
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllConc)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      !������� ��������� �������� ���� �� ����� REPRORYV (����� �� ������� �����)
      if (isTEST >= 2) write(333, *) 'Delete mass mass TempFileNameOutputAllMass=',trim(TempFileNameOutputAllMass)
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllMass)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
    end if
    if (l.EQ.NDTnum) then  ! ��� �� ��������� ������ � ��������
      !�������� ����� � �������������� � ������� � ����� ���� 1_mass_2outl.txt
      AddedStr = AddEndOfOutputFile(2) !_2outl
      command = 'copy "'//trim(TempFileNameOutputAllConc)//'" &
       & recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_conc'//AddedStr//'.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      command = 'copy "'//trim(TempFileNameOutputAllMass)//'" &
       & recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_mass'//AddedStr//'.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      !������ ����� � �������
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllConc)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllMass)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
    end if
    if (l.EQ.(NDTnum+1)) then  ! ��� �� ��������� ������ � �������� + �������� 30 ����
      !�������� ����� � �������������� � ������� � ����� ���� 1_mass_3cool.txt
      AddedStr = AddEndOfOutputFile(3) !_3cool
      command = 'copy "'//trim(TempFileNameOutputAllConc)//'" &
       & recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_conc'//AddedStr//'.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      command = 'copy "'//trim(TempFileNameOutputAllMass)//'" &
       & recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_mass'//AddedStr//'.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      !������ ����� � �������
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllConc)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllMass)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
    end if
    if (l.EQ.(NDTnum+2)) then  ! ��� �� ��������� ������ � �������� + �������� 30 ���� + �������� 2-3 ���� � ���
      !�������� ����� � �������������� � ������� � ����� ���� 1_mass_4recl.txt
      AddedStr = AddEndOfOutputFile(4) !_4recl
      command = 'copy "'//trim(TempFileNameOutputAllConc)//'" &
       & recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_conc'//AddedStr//'.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      command = 'copy "'//trim(TempFileNameOutputAllMass)//'" &
       & recycle_bpsd\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'_mass'//AddedStr//'.txt 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      !������ ����� � �������
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllConc)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
      command = trim(DelComm)//' "'//trim(TempFileNameOutputAllMass)//'" 1>>tmp 2>&1'
      if (isTEST >= 3) write(333, *) trim(command)
      re_i = system(command)
    end if
    l = l + 1
  end do



  !�� ����� ������������ �� ��������� ������ ������� (_1load) ������ ����� ��, �� ��� �����
  AddedStr = AddEndOfOutputFile(1) !_1load
  TempFileNameOutputAllConc = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\' &
     & //trim(StepGlobal_STR)//'_conc'//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'TempFileNameOutputAllConc 1st=',trim(TempFileNameOutputAllConc)
  open (603,file=trim(TempFileNameOutputAllConc),status='unknown')
  TempFileNameOutputAllMass = 'recycle_bpsd\'//trim(StepGlobal_STR)//'stp\' &
    & //trim(StepGlobal_STR)//'_mass'//trim(AddedStr)//'.txt'
  if (isTEST >= 2) write(333, *) 'TempFileNameOutputAllMass 1st=',trim(TempFileNameOutputAllMass)
  open (604,file=trim(TempFileNameOutputAllMass),status='unknown')
21  read (603,'(a150)', end=25) StrFile1
  !���� �� ����� �� ����
  if (StrFile1(1:11).eq.'       ����') then
    read (StrFile1,'(a11,I5)') Str11, NumZonIn !      ����    1.
    if (isTEST >= 2) write(333, *) 'NumZonIn= ',NumZonIn
    read (603,'(a6,I4,a150)') Str6,ZoneTypeIn, Str150 ! TYPE:            1
    if (ZoneTypeIn == 3) then
      goto 21 !��������������� ��������� ����������
    end if
    !���������� �������� ���� ��� �������� ���� � ������ ����
    write(604,*) '======================================='
    write(604,'(a150)') StrFile1 !       ����    1.
    write(604,'(a6,I4)') Str6,ZoneTypeIn !TYPE:            1
    read (603,'(a150)') StrFile1  !'Loaded on Step ', StepGlobal
    write(604, '(a150)') StrFile1
    !��������� �������� ������������
    do i=1,Ner1 !��� ������� ������� ������ ��� ������������
      ConcIn = 0
      MassIn = 0
      read (603,'(i3.3, e14.5)') IsotID, ConcIn
      !��� ��������� ������� ������������ �����
      if (ZonesVol(NumZonIn) > 0 ) then
        !�������� ������������� �������� �������
        isot_id = ConcInCaption(i)
        !������� ������� ����� �������� �������
        atomic_mass = GetIsotopeAtomicMass(isot_id)
        !������� ������������ �� ����� �������� �������
        MassIn = ConcIn/CONST_NA*(0.001*CONST_barn*atomic_mass*ZonesVol(NumZonIn))
      end if
      write(604,'(i3.3, e14.5)') IsotID, MassIn
    end do
    write (604,'(a4, e14.5)') 'VOL:',ZonesVol(NumZonIn)
    write (604,*)
    write (604,*)
  end if
  goto 21
25  close(603)
  close(604)


  command = trim(DelComm)//' bpsderr.txt 1>>tmp 2>&1'
  re_i = system(command)
  command = trim(DelComm)//' bpsdlog.txt 1>>tmp 2>&1'
  re_i = system(command)
  command = trim(DelComm)//' BPSD_INPUT.txt 1>>tmp 2>&1'
  re_i = system(command)
  command = trim(DelComm)//' BPSD_Input_conc.txt 1>>tmp 2>&1'
  re_i = system(command)
  command = trim(DelComm)//' BPSD_OUTPUT.txt 1>>tmp 2>&1'
  re_i = system(command)


  !������������ �� ���� ���� ��� �������
  deallocate(ZonesVol)


  deallocate(SigmaF)
  deallocate(SigmaC)
  deallocate(SigmaN2N)
  deallocate(SigmaN3N)


  deallocate(ConcInArrBPSD)
  deallocate(MassInArrBPSD)
  deallocate(ConcInCaption)
  deallocate(DeviationArrBPSD)


  deallocate(NuclDataBPSD)

  deallocate(time_in)
  deallocate(dtime_in)
end subroutine BPSDProcedure
