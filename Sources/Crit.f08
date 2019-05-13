!��������� ����� ������� ���� ��� JARFR ��� ������� �������� ������������
subroutine Crit
  use CommonModJARFR

  !���������� ������ AD - ������������ ������� �� ������� ��������
  real, allocatable :: ADNEWx2(:,:,:)
  character*35 HOUR1, HOUR2     !��������� ����� ������� � �������� ����� ��� �������
  character*180 InpFileName
  !�������� �������� ���������� ���������� ������� �� �������� ����� JARFR
  integer :: Maxi_ORIGINAL
  real :: Eps1_ORIGINAL, Eps2_ORIGINAL, Eps3_ORIGINAL
  !�������� Keff �� ������ �� �������� � ��� (��� �� �����)
  real, allocatable :: KeffArr(:,:), KDaysArr(:)
  !������� �������� ����������� � Keff = 1
  integer :: CurrentIteration=1
  !����� �� �� �� �����������? ����� �� ��������� ���������?
  integer :: IsOKEY = 0
  !�������� ����� ����������� ����������� ��������� Keff � �������� �� ������ ��������. ��� �������� � ����� ������������ � CritEpsilon
  real, allocatable :: MinDeltaFact(:,:)
  !����������� MinDeltaFact �� ����������� �� ������ ����
  real :: MinDeltaFactItog
  !����������� �� ������ ���� � ���� ���� (������ ������������ ����� ������ ���� ���)
  integer, allocatable :: ZonesOfTheYear(:)
  !���, �� ������� ����������� ����������� Keff
  integer :: KeffMinStep
  !����� �� ������ ������������ ��� ������� ������� (������� ������� �� �������� ����� ������� CritIsots + ������ ������� �������� ����)
  integer :: isNeedToChangeConc
  !������������ ���������� �������������� ��������
  integer :: MaxIter = 15



  !������� ������ �������
  allocate(KDaysArr(1:NDTnum+3))
  allocate(ADNEWx2(1:MaxIter, 1:400, 1:99))
  !allocate(ConcIter(1:MaxIter, 1:999))
  allocate(ZonesOfTheYear(1:999))
!  allocate(KeffArr(1:CurrentIteration, 1:NDTnum+3))
  !allocate(MinDeltaFact(1:CurrentIteration))
  allocate(KeffArr(1:MaxIter, 1:NDTnum+3))
  allocate(MinDeltaFact(1:MaxIter, 1:NDTnum+3))
  KeffArr = 0
  ZonesOfTheYear = 0
 !�������� � ���� inp_new_to_run_Crit, ����� ����� ��� ���������
  command = 'copy '//trim(inp_new_to_run)//' inp_new_to_run_Crit.dat 1>>tmp 2>&1'
  re_i = system(command)
  CurrentIteration = 1 !������ ��������


  open(unt1, file = inp_new_to_run, status = 'OLD')
  read(unt1, NML = Mim)
  read(unt1, NML = D26)
  read(unt1, NML = Dan)
  read(unt1, NML = Obr)
  read(unt1, NML = Upbn)
  close(unt1)

  deallocate(ADNEWx2)
  allocate(ADNEWx2(1:MaxIter, 1:M1, 1:Ner1))
!  ConcIter = ConcIter(1:MaxIter, 1:NDTnum+3))
  deallocate(ZonesOfTheYear)
  allocate(ZonesOfTheYear(1:M1))


  write (*,*) 'STARTING CRITICALLY PROCEDURE...'

 !0002 - DELETED

!  write (*,*) 'Reading pre conc'
  !��������� �������� ������������ �� �������� �����
  k = 1
  do i=1,M1
    do j=1,Ner1
      ADNEWx2(CurrentIteration,i,j) = Ad(k)
      k = k + 1
    end do
  end do

  !����������� ��������� ������ ����� �������
!  deallocate(ADNEWx2)


  !��������� �������� �������� ��������� �� �������� ����� ������������
  Maxi_ORIGINAL=Maxi
  Eps1_ORIGINAL=Eps1
  Eps2_ORIGINAL=Eps2
  Eps3_ORIGINAL=Eps3

  !������ �� ������, ����� ������ ���������
  Maxi=20
  Eps1=1.E-3
  Eps2=1.E-3
  Eps3=1.E-3



  !������������ �������

  !��� ����� ���� ������� �������� Keff, ������� �������� �� ��������� �� ����������
777 open(unt1, file = 'inp_new_to_run_Crit.dat', status = 'OLD')
  write (*,*) 'Iteration #',CurrentIteration
  write(unt1, NML = Mim)
  write(unt1, NML = D26)
  write(unt1, NML = Dan)
  write(unt1, NML = Obr)
  write(unt1, NML = Upbn)
  close(unt1)
  !��������, ���� � ��� ������� ������
  command = 'copy inp_new_to_run_Crit.dat '// &
  & 'recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_input_Crit.txt 1>>tmp 2>&1'
  re_i = system(command)



  !��������� JARFR ��� ������ KEFF ��� ������ �� �������
  write(*, *) 'RUNNING JARFR [CRITICAL CORE] FOR STEP '//trim(StepGlobal_STR)//'...'

  call time1 (HOUR1)
  write(*, *) '  '//HOUR1
  InpFileName = trim(mydir)//'\inp_new_to_run_Crit'
  !----------------------------------
  call RunJARFR(InpFileName)
  !----------------------------------

  re_i = system(trim(DelComm)//" inp_new_to_run_Crit_consyst.rez 1>>tmp 2>&1")
  re_i = system(trim(DelComm)//" inp_new_to_run_Crit_RESULTS.rez 1>>tmp 2>&1")
  re_i = system(trim(DelComm)//" inp_new_to_run_Crit_WEIGHTS.rez 1>>tmp 2>&1")
  re_i = system(trim(DelComm)//" inp_new_to_run_RESULTS_DECODED.rez 1>>tmp 2>&1")
  !��������� �������� �������� Keff
!  if (CurrentIteration.EQ.1) then
    command = 'copy inp_new_to_run_Crit_keff.rez '// &
    & 'recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_keff_Crit.txt 1>>tmp 2>&1'
    if (isTEST >= 3) write(333, *) trim(command)
    re_i = system(command)
  !end if
  write(*, *) 'RUNNING JARFR [CRITICAL CORE] FOR STEP '//trim(StepGlobal_STR)//' DONE!'
  call time1 (HOUR2)
  write(*, *) '  '//HOUR2


  !����������� ���������� KEFF �� ���������� ���� �������������
  IsOKEY = 0
  MinDeltaFact(CurrentIteration,:) = 9999.0
  open(222, file = 'inp_new_to_run_Crit_keff.rez', status = 'unknown')
  KeffMinStep = 1
  do j=1,NDTnum+3
    read (222,*) KDaysArr(j), KeffArr(CurrentIteration, j)
    MinDeltaFact(CurrentIteration,j) = KeffArr(CurrentIteration, j) - 1
  end do
  close(222)

  KeffMinStep = 1
  MinDeltaFactItog = -1
  if (abs(MinDeltaFact(CurrentIteration,1)) < abs(MinDeltaFact(CurrentIteration,NDTnum+1))) then
    MinDeltaFactItog = MinDeltaFact(CurrentIteration,1)
    KeffMinStep = 1 !���, �� ������� ����������� ����������� Keff
  else
    MinDeltaFactItog = MinDeltaFact(CurrentIteration,NDTnum+1)
    KeffMinStep = NDTnum+1 !���, �� ������� ����������� ����������� Keff
  end if
  !���������, ����� ��� ��� �� ������ ����� Keff ��� ������ �������
!  if (MinDeltaFact(CurrentIteration,NDTnum+1) < 0) then
    !MinDeltaFactItog = MinDeltaFact(CurrentIteration,NDTnum+1)
    !KeffMinStep = NDTnum+1 !���, �� ������� ����������� ����������� Keff
  !end if
  !!���������, ����� ��� ��� �� ������ ����� Keff ��� ������ �������
  !if (MinDeltaFact(CurrentIteration,1) < 0) then
    !MinDeltaFactItog = MinDeltaFact(CurrentIteration,1)
    !KeffMinStep = 1 !���, �� ������� ����������� ����������� Keff
  !end if
  write (*,*) 'Keff min on step ',KeffMinStep,' = ',KeffArr(CurrentIteration, KeffMinStep)


  !���� ���� �� �� ����� �� ����� ������� Keff - 1 < CritEpsilon, �� �� ����, ���� �� ���, �� ����� ������������� ������
  if (abs(MinDeltaFactItog) < CritEpsilon) then
    !if (MinDeltaFactItog >= 0) then
      IsOKEY = 1
    !end if
  end if


  !���� �� ����, �� ����� ������ ������������
  if (IsOKEY.NE.1) then
    !write (*,*) 'NOT OKEY, do the Crit steps'
    !�� ������ �������� ������ �������� ������������ �� ������ ���� ����, ��������� Keff ������� ������ ������� (� ��� �� MinDeltaFact)
    if (CurrentIteration.EQ.1) then
      n = 1
      do i=1,M1
        do j=1,Ner1
          isNeedToChangeConc = 0
          !������ ��� ������ ��� �������� �� ������ CritIsots + ������ ��� ������ ��� �����������
          if ((CritIsots(j).EQ.1).AND.(RECL_FizStepLoaded(i) == StepGlobal)) then
            isNeedToChangeConc = 1
          end if
          if (isNeedToChangeConc == 1) then
            ADNEWx2(CurrentIteration+1,i,j) = ADNEWx2(CurrentIteration,i,j) - &
                                               & MinDeltaFactItog*ADNEWx2(CurrentIteration,i,j)
          else
            !��� ��� ��������, ������� �� �� �������, �� ��������� �������� ������������
            ADNEWx2(CurrentIteration+1,i,j) = ADNEWx2(CurrentIteration,i,j)
          end if
          n = n + 1
        end do
      end do
    !�� ����������� ��������� ������ ��� �� ���������� ����, ������������ ��� ����������� ����������������
    !� ��������� ������������ �� ���������� ���������
    else
      n = 1
      do i=1,M1
        do j=1,Ner1
          isNeedToChangeConc = 0
          !������ ��� ������ ��� �������� �� ������ CritIsots + ������ ��� ������ ��� �����������
          if ((CritIsots(j).EQ.1).AND.(RECL_FizStepLoaded(i) == StepGlobal)) then
            isNeedToChangeConc = 1
          end if
          if (isNeedToChangeConc == 1) then
            !������� ������������ ������ �� ������������ ���������������� � ��������� ������ ������������
            !(������� �� ����������� �������, ���. ��������� �� ��������� ��������� y=kx+b)
            if (KeffArr((CurrentIteration-1),KeffMinStep).NE.KeffArr(CurrentIteration, KeffMinStep)) then
              ADNEWx2(CurrentIteration+1,i,j) = ADNEWx2(CurrentIteration-1,i,j) + &
              & (ADNEWx2(CurrentIteration-1,i,j) - ADNEWx2(CurrentIteration,i,j))*(1 - &
              & KeffArr((CurrentIteration-1), KeffMinStep))/(KeffArr((CurrentIteration-1),KeffMinStep)- &
              & KeffArr(CurrentIteration, KeffMinStep))
            else
              ADNEWx2(CurrentIteration+1,i,j) = ADNEWx2(CurrentIteration,i,j) - &
              & (ADNEWx2(CurrentIteration,i,j)*MinDeltaFactItog)
            end if
          else
            !��� ��� ��������, ������� �� �� �������, �� ��������� �������� ������������
            ADNEWx2(CurrentIteration+1,i,j) = ADNEWx2(CurrentIteration,i,j)
          end if
          n = n + 1
        end do
      end do
    !else if (CurrentIteration.EQ.1)
    end if


    CurrentIteration = CurrentIteration + 1


    !����� ��������� ��������� ������������ ������������ � � �������� ���������� ������
    n = 1
  !  Ad=Ad(1:(M1*Ner1))
    do i=1,M1
      do j=1,Ner1
        Ad(n) = ADNEWx2(CurrentIteration,i,j)
        n = n + 1
      end do
    end do


    !������������ �� ������ ������������� ��������
    if (CurrentIteration >= MaxIter) then
      write (*,*) 'MAX ITERATIONS IS ACHIVED! Saving inp_new_to_run file to run JARFR'
      goto 778
    else
      write (*,*) 'NEW CONC SAVED, GOTO NEXT ITER STEP...'
      goto 777
    end if

  !end if (IsOKEY.NE.1) then
  end if


  write (*,*) 'CRIT IS ACHIVED SUCCESSFULLY! Saving inp_new_to_run file to run JARFR'
    !���� �� ����, �� �������� ��� ���� � �������� ����, ����� JARFR ������ ��� ��������
!    command = 'copy inp_new_to_run_Crit.dat '//trim(inp_new_to_run)//' 1>>tmp 2>&1'
    !re_i = system(command)
 778   open(unt1, file = inp_new_to_run, status = 'OLD')
    !��������������� ��������
  Maxi=Maxi_ORIGINAL
  Eps1=Eps1_ORIGINAL
  Eps2=Eps2_ORIGINAL
  Eps3=Eps3_ORIGINAL
  write(unt1, NML = Mim)
  write(unt1, NML = D26)
  write(unt1, NML = Dan)
  write(unt1, NML = Obr)
  write(unt1, NML = Upbn)
  close(unt1)

  !write (*,*) 'SAVING NEW CONC IN Recycle folder in _conc file' 00001
  !*DELETED


  re_i = system(trim(DelComm)//" inp_new_to_run_Crit_keff.rez 1>>tmp 2>&1")
  re_i = system(trim(DelComm)//" inp_new_to_run_Crit.dat 1>>tmp 2>&1")
  deallocate(KDaysArr)
  deallocate(KeffArr)
  deallocate(MinDeltaFact)
  deallocate(ADNEWx2)

end subroutine
