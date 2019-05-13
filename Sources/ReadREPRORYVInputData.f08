!������ �������� ������ JARFR (NAMELIST� MIM, D26, DAN � �.�. �������� �������������) � REPRORYV
subroutine ReadREPRORYVInputData
    ! ���������� �� JARFR ������� � ���� ������
    use CommonModJARFR
    !&RECLNAME
    allocate(TypeRecl(1:99999))
    allocate(Kclear(1:99999))
    allocate(GroupRecl(1:99999))
    allocate(GrReclFraq(1:99999))
    allocate(NkRecl(1:99999))
    allocate(BpsdModeIsots(1:99999))
    allocate(DensChange(1:99999))
    allocate(CritIsots(1:99999))

    !&RECLNAME
    !������� ������� ��� �������� ������ �� ��������
    open(unt1, file = inpRECL_temp, status = 'UNKNOWN')
    !�������������� ���������� ��� �������� ������ �����
    !����������� �� ���� -1
    GroupRecl=-1
    GrReclFraq=-1
    TypeRecl=-1
    NkRecl=-1
    Kclear=-1
    READ(unt1, NML = RECLNAME)
    !���� ���-�� ����������� ��������� GroupRecl
    do i=1,99999
      if (GroupRecl(i).EQ.-1) then
        exit
      end if
    end do
    SizeOfGroupRecl = i - 1
    if (isTEST >= 2) write (*,*) 'SizeOfGroupRecl=',SizeOfGroupRecl
    !���� ���-�� ����������� ��������� GroupRecl
    do i=1,99999
      if (TypeRecl(i).EQ.-1) then
        exit
      end if
    end do
    SizeOfTypeRecl = i - 1
    if (isTEST >= 2) write (*,*) 'SizeOfTypeRecl=',SizeOfTypeRecl
    !���� ���-�� ����������� ��������� NkRecl
    do i=1,99999
      if (NkRecl(i).EQ.-1) then
        exit
      end if
    end do
    SizeOfNkRecl = i - 1
    if (isTEST >= 2) write (*,*) 'SizeOfNkRecl=',SizeOfNkRecl
    !���� ������������ ������ ��������, ����� �������� ������ ��� �������
    MaxGroupRecl = 0
    do i=1,Ner1
      if (GroupRecl(i) > MaxGroupRecl) MaxGroupRecl = GroupRecl(i)
    enddo
    !���� ������������ �������� ����� ���������� - ������ ��� 6 �������������=6 ��� �� ������ ���������� �������
    i = 1;
    maxNkRECL = 0;
    do while (i <= NkDimention)
      if (NkRecl(i) > maxNkRECL) maxNkRECL = NkRecl(i)
      i = i + 1
    enddo
    MkCamp = maxNkRECL !���-�� ������������� - ��� ������������ ����� ��� �������� ������� � �������� ���� �� ����������
    close(unt1)
    deallocate(GroupRecl)
    deallocate(GrReclFraq)
    deallocate(TypeRecl)
    deallocate(Kclear)
    deallocate(NkRecl)
    deallocate(BpsdModeIsots)
    deallocate(DensChange)
    deallocate(CritIsots)
    open(unt1, file = inpRECL_temp, status = 'UNKNOWN')
    allocate(GroupRecl(1:Ner1))
    allocate(GrReclFraq(1:Ner1))
    allocate(BpsdModeIsots(1:Ner1))
    allocate(DensChange(1:Ner1))
    allocate(TypeRecl(1:MaxGroupRecl))
    allocate(Kclear(1:MaxGroupRecl))
    allocate(NkRecl(1:NkDimention))
    allocate(CritIsots(1:Ner1))
    READ(unt1, NML = RECLNAME)
    close(unt1)
    !if (isTEST >= 2) write (*,*) 'maxNkRECL=',maxNkRECL;
end subroutine
