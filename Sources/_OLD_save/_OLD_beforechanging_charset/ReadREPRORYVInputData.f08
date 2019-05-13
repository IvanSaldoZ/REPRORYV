!ЧИТАЕМ ИСХОДНЫЕ ДАННЫЕ JARFR (NAMELISTы MIM, D26, DAN и т.д. придется воспроизвести) И REPRORYV
subroutine ReadREPRORYVInputData
    ! переменные из JARFR описаны в этом модуле
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
    !НАХОДИМ СНАЧАЛА ВСЕ ЧИСЛОВЫЕ ДАННЫЕ ПО МАССИВАМ
    open(unt1, file = inpRECL_temp, status = 'UNKNOWN')
    !Инициализируем переменные для проверки ошибок ввода
    !Присваеваем им всем -1
    GroupRecl=-1
    GrReclFraq=-1
    TypeRecl=-1
    NkRecl=-1
    Kclear=-1
    READ(unt1, NML = RECLNAME)
    !ищем кол-во заполненных элементов GroupRecl
    do i=1,99999
      if (GroupRecl(i).EQ.-1) then
        exit
      end if
    end do
    SizeOfGroupRecl = i - 1
    if (isTEST >= 2) write (*,*) 'SizeOfGroupRecl=',SizeOfGroupRecl
    !ищем кол-во заполненных элементов GroupRecl
    do i=1,99999
      if (TypeRecl(i).EQ.-1) then
        exit
      end if
    end do
    SizeOfTypeRecl = i - 1
    if (isTEST >= 2) write (*,*) 'SizeOfTypeRecl=',SizeOfTypeRecl
    !ищем кол-во заполненных элементов NkRecl
    do i=1,99999
      if (NkRecl(i).EQ.-1) then
        exit
      end if
    end do
    SizeOfNkRecl = i - 1
    if (isTEST >= 2) write (*,*) 'SizeOfNkRecl=',SizeOfNkRecl
    !Ищем максимальную группу изотопов, чтобы выделить память для массива
    MaxGroupRecl = 0
    do i=1,Ner1
      if (GroupRecl(i) > MaxGroupRecl) MaxGroupRecl = GroupRecl(i)
    enddo
    !ищем максимальное значение годов перегрузки - обычно это 6 микрокампаний=6 лет на полное обновление топлива
    i = 1;
    maxNkRECL = 0;
    do while (i <= NkDimention)
      if (NkRecl(i) > maxNkRECL) maxNkRECL = NkRecl(i)
      i = i + 1
    enddo
    MkCamp = maxNkRECL !кол-во микрокампаний - это максимальное число лет хранения топлива в акитвной зоны до перегрузки
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
