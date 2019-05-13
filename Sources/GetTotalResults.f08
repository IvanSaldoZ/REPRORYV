!�������� ������������� ���������, ��������� ��� �������� ����� JARFR �� ���� �����
subroutine GetTotalResults
  use CommonModJARFR
  character*250 KeffOutPutFile, ResultAllOutPutFile, ResultAllOutPutFile_TEMP
  character*250 :: KVFileName, KrFileName
  character*160 StrFile1 !������ ��� ������ �����
  real DaysCurrent
  real, allocatable :: KDaysArr(:), KeffArr(:), KrArr(:), KVcoreArr(:), KVreactorArr(:), &
                   &   KDaysArr2(:), KReactivityArr(:)
  character*4 StepIn_STR
  character*6 AddedStr, AddEndOfOutputFile
  integer count_num !������ (1) ��� ����� (3) ����
  real MassPu239and240_1, MassPuAll_1, MassU235_1, MassU238_1 !�������� ���� Pu, U �� ���� ��������
  real MassPu239and240_3, MassPuAll_3, MassU235_3, MassU238_3 !�������� ���� Pu, U �� ���� ��������
  real zero1,zero2,zero3
  character*4 fmtStr
  character*25 str25
  integer MkCampCounterIn


  write(*, *) 'SAVING TOTAL INFORMATION...'
  open (778, file='recycle/alldata.txt', status='unknown')
  write (778,*) '========================='
  write (778,*) 'REPRORYV OUTPUT DATA FILE'
  write (778,*) "VERSION: ",CONST_VERSION;
  write (778,*) "DATE RELEASE: ",CONST_DATE;
  write (778,*) "(C) IVAN SALDIKOV";
  write (778,*) '========================='
  write (778,'(/,a32)') 'NOTE: ALL MASSES ARE IN KILOGRAMS'
  !write (778,'(/,a82)') 'STEP   MKK      DAYS      KEFF      PU_TOTAL      PU(9+1)       U238          U235'
  write (778,'(/,a138)') 'STEP   MKK      DAYS         KEFF       KeffRe      KVcore   KVreactor'// &
                      &  '         KrV      PU_TOTAL       PU(9+1)          U238          U235'
  DaysCurrent = 0
  MkCampCounterIn = 1
  count_num = 1 !������ ����
  allocate(KDaysArr(1:NDTnum+2))
  allocate(KDaysArr2(1:NDTnum+2))
  allocate(KeffArr(1:NDTnum+2))
  allocate(KVcoreArr(1:NDTnum+2))
  allocate(KVreactorArr(1:NDTnum+2))
  allocate(KrArr(1:NDTnum+2))
  allocate(KReactivityArr(1:NDTnum+2))
  KReactivityArr = 0
  i = 1
  do while (i<=(StepGlobal-1))
    !���������� ������� ��� � ������
    SELECT CASE (i)
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
    write (unit=StepIn_STR, FMT=fmtStr) i

    !KEFF, ���
    KeffOutPutFile = 'recycle\'//trim(StepIn_STR)//'stp\'//trim(StepIn_STR)//'stp_jarfr_keff.txt'
    open (111,file=KeffOutPutFile,status='unknown')
    do j=1,NDTnum+2
      read (111,*) KDaysArr(j), KeffArr(j)
    end do
    close(111)
    !if (isTEST >= 2) write(*, *) 'KeffOutPutFile=',KeffOutPutFile
    !if (isTEST >= 2) write(*, *) 'KeffBegin=',KeffBegin


    !KVcore, KVreactor
    KVFileName = 'recycle\'//trim(StepIn_STR)//'stp\'//trim(StepIn_STR)//'_KV.txt'
    open (114,file=KVFileName,status='unknown')
    read (114,*) !  �� - ����������� ��������������� �������� ���� (��) � �������� (�-�) �� �����, NC/NCF
    read (114,*) !       i      KVCore   KVreactor
    do j=1,NDTnum+2
      read (114,'(i5,2f12.5)') k, KVcoreArr(j), KVreactorArr(j)
    end do
    close(114)


    !Kr
    KrFileName = 'recycle\'//trim(StepIn_STR)//'stp\'//trim(StepIn_STR)//'_kr.txt'
    open (113,file=KrFileName,status='unknown')
    read (113,*) ! Kr - ����������� ��������������� �� V �� �����
    do j=1,NDTnum+2
      read (113,'(i5,f12.5)') k, KrArr(j)
    end do
    close(113)



    !KeffReactivity
    if ((DoplerOn.EQ.1).OR.(DensKoeffOn.EQ.1)) then
      KrFileName = 'recycle\'//trim(StepIn_STR)//'stp\'//trim(StepIn_STR)//'stp_jarfr_keff_Reactivity.txt'
      open (115,file=KrFileName,status='unknown')
      read (115,*) ! Kr - ����������� ��������������� �� V �� �����
      do j=1,NDTnum+2
        read (115,*) KDaysArr2(j), KReactivityArr(j)
      end do
      close(115)
    end if




    !�����
    ResultAllOutPutFile_TEMP = 'recycle\'//trim(StepIn_STR)//'stp\'//trim(StepIn_STR)//'_mass'
11  AddedStr = AddEndOfOutputFile(count_num)
    ResultAllOutPutFile = trim(ResultAllOutPutFile_TEMP)//trim(AddedStr)//'.txt'
    open (112,file=ResultAllOutPutFile,status='unknown')
    !if (isTEST >= 2) write(*, *) 'ResultAllOutPutFile=',ResultAllOutPutFile
10  read (112,'(a160)', end=15) StrFile1
    if (StrFile1(1:56).eq.'           �������� ��������� �������� �� ����� ��������') then
      !if (isTEST >= 2) write(*, *) 'FOUND'
      read (112,'(a160)') !
      read (112,'(a160)') !                         ���.����      ���.�����     ���.�����      �������
      if (count_num == 1) then
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU235_1
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU238_1
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPuAll_1
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPu239and240_1
      end if
      if (count_num == 3) then
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU235_3
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassU238_3
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPuAll_3
        read (112,'(a25,4f14.4)') str25, zero1, zero2, zero3, MassPu239and240_3
      end if
      goto 10
    else
      goto 10
    end if
15  close(112)
    if (count_num == 1) then
      count_num = 3  !���� ������ �� ������ �������� �� ���� + ���������� ���
      goto 11
    end if
    if (count_num == 3) then
      count_num = 1 !������������ �� ��������� ������ �������
    end if
    !��������� ��� ���������� ������ � �������������� ����
    do j=1,NDTnum+2
      str25 = ''
      if (j == 1) then
        write (778,'(i4, a3, i3, f10.2, 2f13.5, 3f12.5, 4e14.5)') i, str25, MkCampCounterIn, DaysCurrent, KeffArr(j), &
                    & KReactivityArr(j), KVcoreArr(j), KVreactorArr(j), KrArr(j), MassPuAll_1, MassPu239and240_1,    &
                    & MassU238_1, MassU235_1
      else
        if (j<NDTnum+2) then
          write (778,'(i4, a3, i3, f10.2, 2f13.5, 3f12.5)') i, str25, MkCampCounterIn, DaysCurrent+KDaysArr(j), KeffArr(j), &
                                  &  KReactivityArr(j), KVcoreArr(j), KVreactorArr(j), KrArr(j)
        else
          write (778,'(i4, a3, i3, f10.2, 2f13.5, 3f12.5, 4e14.5)') i, str25, MkCampCounterIn, DaysCurrent+KDaysArr(j), &
            & KeffArr(j),  KReactivityArr(j), KVcoreArr(j), KVreactorArr(j), KrArr(j), MassPuAll_3, MassPu239and240_3, &
            & MassU238_3, MassU235_3
          MkCampCounterIn = MkCampCounterIn + 1 !������� �������������
          if (MkCampCounterIn > MkCamp) MkCampCounterIn = 1
          DaysCurrent = DaysCurrent + KDaysArr(j)
          i = i + 1 !��������� ���
        end if
      end if
    end do
  end do
  close(778)
  deallocate(KDaysArr)
  deallocate(KeffArr)
  deallocate(KVcoreArr)
  deallocate(KVreactorArr)
  deallocate(KrArr)
  deallocate(KDaysArr2)
  deallocate(KReactivityArr)
  write(*, *) 'SAVING TOTAL INFORMATION DONE!'
end subroutine
