!������������ ���� �� ������ ����� ����
subroutine PreStepShow
  use CommonModJARFR
  character*4 fmtStr

  SELECT CASE (StepGlobal)
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
  write (unit=StepGlobal_STR, FMT=fmtStr) StepGlobal !��������� ����� StepGlobal � ������ StepGlobal_STR
  write(*, *) ''
  write(*, *) ''
  write(*, *) ''
  write(*, *) '======================'
  write(*, *) 'STEP: '//trim(StepGlobal_STR)
  re_i = system("timeout /t 1  1>>tmp 2>&1")
  write(333, *) ''
  write(333, *) ''
  write(333, *) ''
  write(333, *) '======================'
  write(333,*) 'STEP: '//trim(StepGlobal_STR)

  !������� ������� ��� �������� ����
  command = 'mkdir recycle\'//trim(StepGlobal_STR)//'stp\  1>>tmp 2>&1'
  if (isTEST >= 2) write(333, *) command
  re_i = system(command)
  CurDir = 'recycle\'//trim(StepGlobal_STR)//'stp'

end subroutine PreStepShow



!��������� �������� - �������������
subroutine InitialMovements
  use CommonModJARFR
  !���-���� �� ����������� ��������
  open(333, file = "reproryv_log.txt", status = 'UNKNOWN')
  !�������� ������� ���� ��������� REPRORYV � ����� "recycle"
  !������� �������
  command = 'mkdir recycle\  1>>tmp 2>&1'
  if (isTEST >= 2) write(333, *) command
  re_i = system(command)
  command = 'copy '//trim(inpRECL)//' recycle\'//trim(inpRECL)//'  1>>tmp 2>&1'
  if (isTEST >= 2) write(333, *) command
  re_i = system(command)
  command = 'copy '//trim(inp1)//' recycle\'//trim(inp1)//'  1>>tmp 2>&1'
  if (isTEST >= 2) write(333, *) command
  re_i = system(command)
 !��������� ����� �������� ���� � ������, ����� ����� ���� ��������� � �����
  StepGlobal = 1
end subroutine InitialMovements



!��������� ������
!> \brief Checking balance subroutine
!!
subroutine SaveBalance
  use CommonModJARFR
  real CheckBalance
  !��������� ���������� �����
  write(*, *) 'SAVING BALANCE...'
  open(334, file = 'recycle\balance.txt', status = 'UNKNOWN')
  write(334,'(/,a32)') 'BALANCE OF MASSES: PU'
  !write(334,'(/,a87)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL       '
  write(334,'(/,a102)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL    ALL_BEF_CHK'
  do i=1,MkCamp*nCamp
    if (i > nVRH + nRecl + 1) then
      CheckBalance = MassDataPu(i)%mass_after - MassDataPu(i)%mass_unloaded + MassDataPu(i)%mass_recycle_after
    else
      CheckBalance = MassDataPu(i)%mass_after - MassDataPu(i)%mass_unloaded + MassDataPu(i+1)%mass_loaded
    end if
    write (334,'(i4, 7e14.5)') i, MassDataPu(i)%mass_loaded, MassDataPu(i)%mass_before,  &
    & MassDataPu(i)%mass_after,  MassDataPu(i)%mass_unloaded, MassDataPu(i)%mass_recycle_before, &
    & MassDataPu(i)%mass_recycle_after, CheckBalance
  end do
  write(334,'(/,a32)') 'BALANCE OF MASSES: U'
!  write(334,'(/,a87)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL       '
  write(334,'(/,a102)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL    ALL_BEF_CHK'
  do i=1,MkCamp*nCamp
    if (i > nVRH + nRecl + 1) then
      CheckBalance = MassDataU(i)%mass_after - MassDataU(i)%mass_unloaded + MassDataU(i)%mass_recycle_after
    else
      CheckBalance = MassDataU(i)%mass_after - MassDataU(i)%mass_unloaded + MassDataU(i+1)%mass_loaded
    end if
    write (334,'(i4, 7e14.5)') i, MassDataU(i)%mass_loaded, MassDataU(i)%mass_before,  &
    & MassDataU(i)%mass_after,  MassDataU(i)%mass_unloaded, MassDataU(i)%mass_recycle_before, &
    & MassDataU(i)%mass_recycle_after, CheckBalance
  end do


  write(334,'(/,a60)') 'VECTOR OF PLUTONIUM AT THE END OF THE STEP: TOTAL'
!  write(334,'(/,a87)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL       '
  write(334,'(/,a88)') '#STEP  236           238           239           240           241           242        '
  do i=1,MkCamp*nCamp
    write (334,'(i4, 6e14.5)') i, PuVector(i)%Pu236, PuVector(i)%Pu238,  &
    & PuVector(i)%Pu239,  PuVector(i)%Pu240, PuVector(i)%Pu241, &
    & PuVector(i)%Pu242
  end do


  write(334,'(/,a60)') 'VECTOR OF URANIUM AT THE END OF THE STEP: TOTAL'
!  write(334,'(/,a87)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL       '
  write(334,'(/,a60)') '#STEP  234           235           236           238        '
  do i=1,MkCamp*nCamp
    write (334,'(i4, 4e14.5)') i, UVector(i)%U234, UVector(i)%U235,  &
    & UVector(i)%U236,  UVector(i)%U238
  end do


  write(334,'(/,a60)') 'VECTOR OF PLUTONIUM AT THE END OF THE STEP: LOADED'
!  write(334,'(/,a87)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL       '
  write(334,'(/,a88)') '#STEP  236           238           239           240           241           242        '
  do i=1,MkCamp*nCamp
    write (334,'(i4, 6e14.5)') i, PuVector_LOADED(i)%Pu236, PuVector_LOADED(i)%Pu238,  &
    & PuVector_LOADED(i)%Pu239,  PuVector_LOADED(i)%Pu240, PuVector_LOADED(i)%Pu241, &
    & PuVector_LOADED(i)%Pu242
  end do


  write(334,'(/,a60)') 'VECTOR OF URANIUM AT THE END OF THE STEP: LOADED'
!  write(334,'(/,a87)') '#STEP  LOADED        ALL_BEFORE    ALL_AFRTER    UNLOADED      BEFORE_RECL   AFTER_RECL       '
  write(334,'(/,a60)') '#STEP  234           235           236           238        '
  do i=1,MkCamp*nCamp
    write (334,'(i4, 4e14.5)') i, UVector_LOADED(i)%U234, UVector_LOADED(i)%U235,  &
    & UVector_LOADED(i)%U236,  UVector_LOADED(i)%U238
  end do
  close(334)

  write(*, *) 'SAVING BALANCE DONE!'
end subroutine SaveBalance



!��������� ��������� JARFR �� ����
subroutine RunJARFR(inpfilename)
    use CommonModJARFR
    character*180 :: inpfilename
    if (isTEST >= 2) write(333,*) 'call "'//trim(JARFRbatfilename)//'" '//trim(inpfilename)//' 1>jarlog.txt 2>&1'
    re_i = system('call "'//trim(JARFRbatfilename)//'" '//trim(inpfilename)//' 1>jarlog.txt 2>&1')
    if (isTEST >= 2) write(333,*) 'cd /d %~dp0 1>>tmp 2>&1'
    re_i = system('cd /d %~dp0 1>>tmp 2>&1');
end subroutine



!������� ������� � ���, ��� �� ��� ������� �� �������� ����� ��������� JARFR
subroutine InputFileDelDollars(inpin,inpout)
    use CommonModJARFR
    character*60 :: inpin, inpout
    !���������� ��� ������
    integer j
    !����� � ������ ������
    character*250 :: str1;
    !����� �������, � �������� ���������� ����������� � JARFR
    integer DollarIndex;
    !ios - ��� ������� ����� �����
    integer ios;
    open(unt1, file = inpin, status = 'old');
    open(unt2, file = inpout, status = 'UNKNOWN');
    ios = 0
    !iostat:
      ! = -1 error: end of file
      ! = -2 error: end of record
    !������ ���-�� ����� � �����
    do while (ios /= -1)
        read(unt1, '(A)', advance = "yes", iostat = ios) str1
        if (ios /= -1) then
          DollarIndex = len_trim(str1) !���������� ����� ������ ��� ������� � ��� �������
          do j=1,len_trim(str1)
            if (str1(j:j) == '$') then
              DollarIndex=j-1
              EXIT
            end if
          end do
          !���������� ����� ������ �� 1-�� �� ����� �������
          !write(*, *) str1(1:MAXLEN)
         write(unt2, *) str1(1:DollarIndex)
        end if
    end do
    close(unt2) ! ��������� ����� ����
    close(unt1) ! ��������� �������� ���� JARFR
end subroutine



!������� ��� ������ ��������� �����, � ������� ����� �������� ���������� ��� ������� �������� ������ ����
!inCount - ����� ������� (1, 2, 3, 4) - 1 ������ ���� StepNum, 2 - ����� 330 �����, 3 - ����� 360 �����, 4 - ����� 1455.0, �.�. ������ ��������� �������� ������� � �������
function AddEndOfOutputFile(inCount)
  character*6 AddEndOfOutputFile
  character*6 AddedStr1
  integer inCount
  SELECT CASE (inCount)
   CASE (1)
      AddedStr1 = '_1load' !����������� �������
   CASE (2)
      AddedStr1 = '_2outl' !����������� �������
   CASE (3)
      AddedStr1 = '_3cool'  !����� ���������� ����������
   CASE (4)
      AddedStr1 = '_4recl' !����� ����������� �� ���
   CASE DEFAULT
      AddedStr1 = '_UNKN'
  END SELECT
  AddEndOfOutputFile = AddedStr1
end function AddEndOfOutputFile



!��������� ������� ����� �������� �� ��� ��������������
function GetIsotopeAtomicMass(inCode)
  use CommonModJARFR
  real GetIsotopeAtomicMass
  integer inCode, CurI
  !if (isTEST >= 2) write(*,*) 'GetIsotopeAtomicMass...';
  CurI = -1
  do i=1,99
    if (NuclDataCommon(i)%nucl_numberID == inCode) then
      CurI = i
    end if
  end do
  !if (isTEST >= 2) write(*,*) 'CurI=',CurI;
  if (CurI > -1) then
    GetIsotopeAtomicMass = NuclDataCommon(CurI)%nucl_atomic_mass
  else
    GetIsotopeAtomicMass = 0
  end if
  !if (isTEST >= 2) write(*,*) 'GetIsotopeAtomicMass=',GetIsotopeAtomicMass
end function GetIsotopeAtomicMass



!��������� ����������� ������������� �������� ��� ������� �� ��� ��������������
function GetIsotopeCaption(inCode)
  use CommonModJARFR;
  character*4 GetIsotopeCaption;
  integer inCode, CurI;
  !if (isTEST >= 2) write(*,*) 'GetIsotopeAtomicMass...';
  CurI = -1;
  do i=1,99
    if (NuclDataCommon(i)%nucl_numberID == inCode) then
      CurI = i;
    end if
  end do
  if (CurI > -1) then
    GetIsotopeCaption = NuclDataCommon(CurI)%nucl_Caption
  else
    GetIsotopeCaption = '----';
  end if;
end function GetIsotopeCaption



!�������� ������ ��� ���� ��������, ������������ � ���������
subroutine ALLOCATE_MEMEORY_FOR_ARRAYS
    use CommonModJARFR
    if (StepGlobal==1) then
      !������������� �������� ������ ��� ����������, ����������� ��� ����������
      allocate(RECL_nzon_type(1:99999)); !������� ����, ����� �� ��������� ��� ���-���� ����� ������ ��� �� ţ ��������� ��� ����
      RECL_nzon_type = 1 !����� ���������������� ����� ��, ������ ��� �� ����� ����� �������� � ���� ��������
      RECL_FizMatCounter_GLOBAL = 0
      allocate(RECL_FizOldMatIds_GLOBAL(1:99999))
      allocate(RECL_FizNewMatIds_GLOBAL(1:99999))
      RECL_FizMatCounter_GLOBAL_R = 0
      allocate(RECL_FizOldMatIds_GLOBAL_R(1:999999))
      allocate(RECL_FizNewMatIds_GLOBAL_R(1:999999))
      allocate(RECL_FizStepLoaded(1:9999999))
      allocate(MassDataPu(1:9999999))
      allocate(MassDataU(1:9999999))
    end if
end subroutine



!����������� ������
subroutine DE_ALLOCATE_MEMEORY_OF_ARRAYS
    use CommonModJARFR
    !�������� ������ ��� ����, ����� ��������� ������
    !&MIM
    deallocate(sostav)
    deallocate(nzon)
    deallocate(ad)
    deallocate(tem)
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
end subroutine


!��������� ����������� ������������� �������� ��� ������� �� ��� ��������������
subroutine ShowErrorFromCode(inCode)
  use CommonModJARFR

   write(*,*) "=================================="
  SELECT CASE (inCode)
   CASE (0)
    write(*,*) "No errors found - OK!";
   CASE (1)
    write(*,*) "ERROR 1: (nVRH + nRecl) >= MkCamp.  Time for the recycling is not enough! &
   & Please open the input file of REPRORYV code to correct these variables.";
   CASE (2)
    write(*,*) 'ERROR 2: MkCamp = maxNkRECL. "The count of the microcampaings on the map of &
   & recycling steps (NkRecl) not corresponds to the count of microcampain variable (MkCamp)! &
   & Please open the input file of REPRORYV code to correct these variables."';
   CASE (3)
    !3 - ���-�� ��������� ������ ����������� GroupRecl ������, ��� ���-�� ��������� ������ �������� �� ������� ����� JARFR
    write(*,*) 'ERROR 3: SizeOf(GroupRecl)[REPRORYV] < SizeOf(Sostav)=Ner1[JARFR]';
   CASE (4)
    !4 - ���-�� ��������� ������ ����������� GroupRecl ������, ��� ���-�� ��������� ������ �������� �� ������� ����� JARFR
    write(*,*) 'ERROR 4: SizeOf(GroupRecl)[REPRORYV] > SizeOf(Sostav)=Ner1[JARFR]';
   CASE (5)
    !5 - � ������ TypeRecl ������������ ������ � ������ �������, ������� ��� � ������ GroupRecl �� ������� ����� REPRORYV
    write(*,*) 'ERROR 5: SizeOf(TypeRecl)[REPRORYV] < max(TypeRecl)[REPRORYV]. See input file of REPRORYV';
   CASE (6)
    !6 - � ������ TypeRecl �� ������� ������ � �������, ������� ������������ � ������ GroupRecl �� ������� ����� REPRORYV
    write(*,*) 'ERROR 6: SizeOf(TypeRecl)[REPRORYV] > max(TypeRecl)[REPRORYV]. See input file of REPRORYV';
   CASE (7)
    !7 - ����������� ������� ������ NkRecl �������� ����� REPRORYV ������, ��� ����������� ������� Nk �� �������� ����� JARFR
    write(*,*) 'ERROR 7: SizeOf(NkRecl)[REPRORYV] < SizeOf(Nk)[JARFR]';
   CASE (8)
    !8 - ����������� ������� ������ NkRecl �������� ����� REPRORYV ������, ��� ����������� ������� Nk �� �������� ����� JARFR
    write(*,*) 'ERROR 8: SizeOf(NkRecl)[REPRORYV] > SizeOf(Nk)[JARFR]';
   CASE (9)
    !9 - ���������� ������������� ���������� ��� �� ������ ����� �� ���������
    !������ ����� ���������, ����� ������������ �������� ����������� ����� �������� �� ���� �������� �����
      !   KT= 6, 2, 2, 4, 2, 2, 7,
      !       6, 2, 2, 2, 2, 2, 7,
     !��� ����� � ������ ������ ����������� ���� 2 � 4 (��� ���-����), � �� ������ ������ 2 (���� ���-����).
     !����� ��������� ���� �����:
      !   KT= 6, 2, 2, 4, 2, 2, 7,
      !       6, 2, 2, 5, 2, 2, 7,
    write(*,*) 'ERROR 9: The quantity of the previously UNloaded fiz zones is NOT equal to the quantity of the loaded fiz zones';
   CASE (10)
    !10 - ��������� ������������� ����� �� ���� �������� ����� ����������
    !������ ����� ���������, ����� ����������� ������ ����� ���������� �� ���� �������� ����� (���� ���� ���������� ���������� ����������� ���-���) �������� ����������� �� ������ ������ ��� ����� (1-� � 2-� ������), � �� ������ - ������ ���� (3-� ������)
      !   KT= 6, 2, 2, 4, 2, 2, 7,
      !       6, 2, 2, 4, 2, 2, 7,
      !       6, 4, 4, 4, 2, 2, 7,
     !��� ����� � ������ ������ ����������� ������ ��� 4-� ���� � ������ 2-�, ��� ���� � ��������� ��� ����������� ��� 4-� ���� � ����� ��� 2-�.
     !����������, ��� ��������� ������� ������ ���������, ��� ����� ����� ����� ���������
     !����� ��������� ���� ����� (�������� �ݣ ���� ����������� ����� � ��������� ţ ����):
      !   KT= 6, 2, 2, 4, 2, 2, 7,
      !       6, 2, 2, 4, 2, 2, 7,
      !       6, 4, 4, 4, 2, 2, 7,
      !       6, 4, 4, 4, 2, 2, 7,
      !���� (�������� ������ ������ �����)
      !   KT= 6, 2, 2, 4, 2, 2, 7,
      !       6, 4, 4, 4, 2, 2, 7,
    write(*,*) 'ERROR 10: Total volume of the previously UNloaded fiz zones is NOT equal to total volume of the loaded fiz zones';

   CASE DEFAULT
    write(*,*) "PROGRAM ENDS WITH ERRORS!";
  END SELECT
  write(*,*) "=================================="

end subroutine ShowErrorFromCode


!�������� �� ������� ������ �� �������� ������
function GetErrors()
  use CommonModJARFR
  integer :: GetErrors, TempErrorCode
  TempErrorCode = 0
  if ((nVRH + nRecl) >= MkCamp) then
    TempErrorCode = 1 !����� �� ����������� ������������
  end if
  if (MkCamp.EQ.0) then
    TempErrorCode = 2 !���� ������ �� ����� �� ���� ��� ����������
  end if
  if (SizeOfGroupRecl.LT.Ner1) then
    TempErrorCode = 3 !���-�� ��������� ������ ����������� GroupRecl ������, ��� ���-�� ��������� ������ �������� �� ������� ����� JARFR
  end if
  if (SizeOfGroupRecl.GT.Ner1) then
    TempErrorCode = 4 !���-�� ��������� ������ ����������� GroupRecl ������, ��� ���-�� ��������� ������ �������� �� ������� ����� JARFR
  end if
  if (SizeOfTypeRecl.LT.MaxGroupRecl) then
    TempErrorCode = 5 !� ������ TypeRecl ������������ ������ � ������ �������, ������� ��� � ������ GroupRecl �� ������� ����� REPRORYV
  end if
  if (SizeOfTypeRecl.GT.MaxGroupRecl) then
    TempErrorCode = 6 !� ������ TypeRecl �� ������� ������ � �������, ������� ������������ � ������ GroupRecl �� ������� ����� REPRORYV
  end if
  if (SizeOfNkRecl.LT.NkDimention) then
    TempErrorCode = 7 !����������� ������� ������ NkRecl �������� ����� REPRORYV ������, ��� ����������� ������� Nk �� �������� ����� JARFR
  end if
  if (SizeOfNkRecl.GT.NkDimention) then
    TempErrorCode = 8 !����������� ������� ������ NkRecl �������� ����� REPRORYV ������, ��� ����������� ������� Nk �� �������� ����� JARFR
  end if
!  if (RECL_ERR_FizZone1.NE.RECL_ERR_FizZone2) then
!    TempErrorCode = 9 !���������� ������������� ���������� ��� �� ������ ����� �� ���������
!  end if
  if (((RECL_ERR_volume1-RECL_ERR_volume2)/RECL_ERR_volume1) > 1E-2) then
    TempErrorCode = 10 !��������� ������������� ����� �� ���� �������� ����� ���������� ����� ��� �� 1 �������
  end if

  GetErrors = TempErrorCode
end function GetErrors



!�������� �������/�������� ����� �� ����� � ����� ������������ �������/�������� ����� �� ����� � ����� ����
subroutine CopyResFilesToRecycleFolder
  use CommonModJARFR
  character*250 InpFile1, OutFile1
  command = 'copy jarlog.txt '//trim(CurDir)//'\'//trim(StepGlobal_STR)//'_jarlog.txt 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = 'copy '//trim(inp_new_to_run)//' '//trim(CurDir)//'\'//trim(StepGlobal_STR)//'stp_jarfr_input.txt 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = 'copy inp_new_to_run_consyst.rez recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_consyst.txt &
  &  1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = 'copy inp_new_to_run_WEIGHTS.rez recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_weights.txt &
  &  1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = 'copy inp_new_to_run_keff.rez recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_keff.txt &
  & 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  !������������ �������� ���� JARFR (��� ����� ������ ���������� DOS-���������, � ����� Windows: CP1251 -> CP866)
  InpFile1 = 'inp_new_to_run_RESULTS.rez'
  OutFile1 = 'inp_new_to_run_RESULTS_DECODED.rez'
  call DecodeFile(InpFile1, OutFile1)
  re_i = system(trim(DelComm)//" inp_new_to_run_RESULTS.rez 1>>tmp 2>&1")
  re_i = system(trim(DelComm)//" inp_new_to_run_WEIGHTS.rez 1>>tmp 2>&1")
  command = 'copy inp_new_to_run_RESULTS_DECODED.rez &
  & recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_results.txt 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
end subroutine CopyResFilesToRecycleFolder


!������� ��� ��������� �������/�������� ����� �� �����
subroutine DelTempFilesAll
  use CommonModJARFR
  command = trim(DelComm)//' '//inp_new_to_run//' 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = trim(DelComm)//' inp_new_to_run_consyst.rez 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = trim(DelComm)//' inp_new_to_run_keff.rez 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = trim(DelComm)//' inp_new_to_run_RESULTS.rez 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = trim(DelComm)//' inp_new_to_run_RESULTS_DECODED.rez 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
  command = trim(DelComm)//' jarlog.txt 1>>tmp 2>&1'
  if (isTEST >= 3) write(333, *) trim(command)
  re_i = system(command)
end subroutine DelTempFilesAll



!������������ ���� �� CP1251 -> CP866
subroutine DecodeFile(inpin,inpout)
    use CommonModJARFR
    use ConvertCyr
    character*250 :: inpin, inpout
    !����� � ������ ������
    character*9999 :: str1, str2
    !ios - ��� ������� ����� �����
    integer ios
    open(unt1, file = inpin, status = 'OLD')
    open(unt2, file = inpout, status = 'UNKNOWN')
    ios = 0
    !iostat:
      ! = -1 error: end of file
      ! = -2 error: end of record
    !������ ���-�� ����� � �����
    do while (ios /= -1)
        read(unt1, '(A)', advance = "yes", iostat = ios) str1
        if (ios /= -1) then
          str2 = DosToWin(str1)
          write(unt2, *) trim(str2)  !���������� ���������������� ������ � ����� ����
        end if
    end do
    close(unt2) ! ��������� ����� ���������������� ����
    close(unt1) ! ��������� �������� �������� ���� JARFR
end subroutine



!��������� ��� ���������� ������� �� �����������
subroutine sort(B,M)
integer M
integer :: B(M)
integer temp
do i=1,M-1
  do J=i+1,M
    if (B(i)>B(j)) then
      temp=B(i)
      B(i)=B(j)
      B(j)=temp
    end if
  end do
end do
end subroutine


!��������� ��������� �������
subroutine time1(hour)
  character*35 hour
  character(len=35) :: date
  call fdate(date)
  hour = date
return
end

