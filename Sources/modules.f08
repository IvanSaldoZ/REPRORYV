module CommonModJARFR
    character*6 :: CONST_VERSION = '0.25.1' !ВЕРСИЯ ПРОГРАММЫ
    character*10 :: CONST_DATE = '2017.01.04' !ДАТА ВЫПУСКА 0.25

    real :: CONST_NA = 6.022E23  ! ЧИСЛО АВОГАДРО [1/моль]
    real :: CONST_barn = 1E24    ! СКОЛЬКО СМ2 В ОДНОМ БАРНЕ [СМ^2]

    !все данные по нуклидам (включая атомный вес, чтобы рассчитать массу/концентрацию)
    type TNuclDataCommon
      integer         nucl_order_num    !Порядковый номер в таблице
      character*4     nucl_Caption      !Название нуклида из базы
      integer         nucl_numberID     !Номер нуклида из базы
      real            nucl_atomic_mass  !Атомный вес
    end type
    type(TNuclDataCommon), allocatable :: NuclDataCommon(:)

    !данные по массовому балансу Pu и U
    type TMassData
      SEQUENCE
      real            mass_loaded         !загруженная масса в АЗ
      real            mass_before         !общая масса в АЗ ДО работы
      real            mass_after          !общая масса в АЗ ПОСЛЕ работы
      real            mass_unloaded       !выгруженная масса из АЗ
      real            mass_recycle_before !масса перед самой переработкой
      real            mass_recycle_after  !масса сразу после переработки (именно она должна быть загружена в активную зону) - ПРОВЕРОЧНЫЙ ФУНКЦИОНАЛ
    end type

    !данные по вектору плутония Pu
    type TPuVector
      SEQUENCE
      real            Pu236
      real            Pu238
      real            Pu239
      real            Pu240
      real            Pu241
      real            Pu242
    end type

    !> Uranium vector
    type TUVector
      SEQUENCE
      real            U234
      real            U235
      real            U236
      real            U238
    end type

    type(TMassData), allocatable :: MassDataU(:)       !масса  Урана
    type(TMassData), allocatable :: MassDataPu(:)      !масса  Плутония
    type(TPuVector), allocatable :: PuVector(:)        !Вектор Плутония ВСЕГО        топлива
    type(TUVector),  allocatable :: UVector(:)         !Вектор Урана    ВСЕГО        топлива
    type(TPuVector), allocatable :: PuVector_LOADED(:) !Вектор Плутония загружаемого топлива
    type(TUVector),  allocatable :: UVector_LOADED(:)  !Вектор Урана    загружаемого топлива



    integer       :: isTEST = 3          !<Show debug info? (for developers only))
    integer       :: re_i                !<I for showing system information
    integer       :: maxNk,NkDimention   !<Максимальный номер в картограмме Nk (нужно для определения размера массива KT в плане)
    integer       :: MaxNSLZ             !Максимальный номер в NSLZ - нужно, чтобы определить размерность массива KT по слоям
    integer       :: maxNkRECL           !Максимальный год, на который происходит перегрузка (из файла программы REPRORY)
    integer       :: StepGlobal          !Текущее значение шага, на котором находится программа
    character*255 :: command             !Сюда можно заносить строку-команду для выполнения через командную строку
    character*4   :: StepGlobal_STR      !Строка текущего шага по выгоранию
    character*250 :: CurDir              !Папка текущего шага
    integer       :: MaxGroupRecl        !Максимальное значение групп изотопов при перегрузке из входного файла REPRORYVа
    integer       :: MkCampCounter=1     !Текущий счетчик микрокампаий (сбрасывается каждй раз при следующей кампании)


    !MIM
    NAMELIST /Mim/ Zsig,m1,m1v,Jpfr,Jdpa,Ner1,Nred,Sostav,Nzon,Ad,Tem,Mng,Mnog,Ncol
    !переменные из Mim
    integer  Zsig,M1,M1v,Jdpa,Ner1,Nred,Mng,Mnog
    integer, allocatable                         :: ncol(:),nzon(:),sostav(:)
    real,    allocatable                         :: Ad(:),                    Tem(:)

    !D26
    NAMELIST /D26/ Ndm,Nt,Nf,Nr2,Nge,Nr1,Nlg,Jitr,H,Nslz,KT,Ns,Nk,Nsm,Hzr,gam,gm,JDRO
    !переменные из D26
    integer  Ndm,Nt,Nf,Nr2,Nge,Nr1,Nlg,Jitr,JDRO
    real     H,gam(1:1300),gm(1:52)
    integer, allocatable                         :: Nslz(:),KT(:),Ns(:),Nk(:),Nsm(:)
    real,    allocatable                         :: Hzr(:)

    !DAN
    NAMELIST /DAN/ Ng,Mg,Nt,Nf,Pw,Juch,J2x,MAXI,Jitr,Eps1,Eps2,Eps3,Nnaz,Ncaz,Jtv, &
    Jupbn,Jfiz,Jnum,Jpri,Ipr,Jprw,JJAP
    !переменные из Dan
    integer Ng,Mg,Juch,J2x,MAXI,Nnaz,Ncaz,Jtv,Jupbn,Jfiz,Jnum,Jpri,Ipr,JJAP
    real Pw,Eps1,Eps2,Eps3
    integer, allocatable :: Jprw(:)

    !OBR
    NAMELIST /OBR/ Naz,Ntor,Nbok,Ngel,Nvel,Ngor,Nvos,Nkv,Ntnz,Jpfz,nkr
    !переменные из Obr
    integer Naz,Ntor,Nbok,Ngel,Nvel,Nkv,nkr
    integer, allocatable :: Ngor(:),Nvos(:),Ntnz(:),Jpfz(:)

    !Upbn
    NAMELIST /UPBN/ Burn_b10,T,Ndt,T_Usr,PWM,Jprn,Jdpas,Jdpt,Ipr,Jprw
    !переменные из Obr
    integer Burn_b10,Ndt,Jdpas
    integer, allocatable :: Jprn(:),Jdpt(:)
    real T
    real, allocatable    :: T_Usr(:),PWM(:)

    !А теперь уже идет входной файл REPRORYV
    !RECLNAME
    NAMELIST /RECLNAME/ nVRH,nRecl,nCamp,TypeRecl,GroupRecl,NkRecl, &
       & TCamp,TDown,StartStep,Kclear,GrReclFraq,NDTnum,BpsdOn,BpsdMode, &
       & DoplerOn, DoplerDelta, DensKoeffOn, DensChange, BpsdModeIsots, &
       & StartStepReactivity, CritOn, CritEpsilon, CritIsots
    integer              :: nVRH,nRecl,nCamp,MkCamp,NDTnum, CritOn
    integer, allocatable :: GroupRecl(:),NkRecl(:),TypeRecl(:)
    real, allocatable    :: Kclear(:),GrReclFraq(:), DensChange(:)
    integer, allocatable :: BpsdModeIsots(:), CritIsots(:)
    character*180        :: JARFRbatfilename
    real TCamp,TDown
    !Точность определения Keff - насколько близко нужно быть к единице по Keff, чтобы считать, что реактор критическим
    real    :: CritEpsilon=0.01
    !Шаг, с которого нужно запускать JARFR
    integer :: StartStep=1
    !Шаг, с которого запускается JARFR для рассчёта эффектов реактивности
    integer :: StartStepReactivity=1
    !нужно ли проводить дополнительный расчёт изменения изотопного состава по bpsd {1 - да, 0 - нет}
    integer :: BpsdOn=0
    !режим получения результата BPSD.
    !    0 - расчёт изотопного состава по BPSD,
    !    1 - расчёт верхнего предела ошибки: обычный результат + ошибка
    !    2 - расчёт нижнего предела ошибки: обычный результата - ошибка.
    integer :: BpsdMode=0
    ! подсчет Доплер эффекта на каждом шаге {1 - включить, 0 - не производить расчёт Допрел-эффекта}
    integer :: DoplerOn = 0
    ! на сколько градусов увеличить температуру топлива, чтобы посчитать эффект Доплера,
    ! например, если DoplerDelta = 300, то при температуре T=1200K, коэффициент размножения будет посчитан при T=1500K
    real    :: DoplerDelta = 300.0
    !подсчет плотностного эффекта реактивности на каждом шаге {1 - включить, 0 - не производить плотностой эффект реактивности}
    integer :: DensKoeffOn = 0

    !Данные по зонам, необходимые для расчёта перегрузок
    !тип физической зоны (в соответствии с Nzon)
    !0 - зона выгружена, использовать нельзя (Disabled) до того года, как она будет перегружена
    !1 - не выгружается из АЗ, обычные зоны для нейтронно-физического расчёта
    !2 - зоны, которые будут переработаны на этом шаге
    !3 - конструкционные материалы (зоны, которые вообще не перегружаются никогда)
    !4 - новая зона для данного шага
    !5 - освободившаяся зона после переработки, можно использовать для нового расчёта
    integer, allocatable  :: RECL_nzon_type(:), RECL_nzon_type_temp(:)

    real, allocatable     :: IsotReclConc(:) !<Concentrations of each isotopes
    integer, allocatable  :: RECL_FizStepLoaded(:)  !<step on which the zone is loaded

    !<Массив статусов (вкл/выкл - 1/0) ТВС. 0 ставится для тех, которые уже вообще не используются (можно их использовать для новой загрузки переработанных ТВС)
    integer, allocatable  :: RECL_TVSnum_Enabled(:)
    !Массив статусов (вкл/выкл - 1/0) физ-зон. 0 ставится для тех физ-зон, которые тоже уже не используются (но их можно использовать для новой загрузки переработанных ТВС)
    !integer, allocatable :: RECL_Fiz_Enabled(:)
    !Массив соответствия новых и старых физических зон для зон, которые становятся вместо выгруженных ТВСок (нужно для поиска исходных концентраций - с самого 1-го шага)  (ГЛОБАЛЬНАЯ ПЕРЕМЕННАЯ)
    integer, allocatable  :: RECL_FizOldMatIds_GLOBAL(:), RECL_FizNewMatIds_GLOBAL(:)
    integer RECL_FizMatCounter_GLOBAL !Общее количество добавленных материалов выгруженных на предыдущем шаге  (ГЛОБАЛЬНАЯ ПЕРЕМЕННАЯ)
    !Массив соответствия новых и старых физических зон для перегружаемых ТВСок (нужно для поиска исходных концентраций - с самого 1-го шага)  (ГЛОБАЛЬНАЯ ПЕРЕМЕННАЯ)
    integer, allocatable  :: RECL_FizOldMatIds_GLOBAL_R(:), RECL_FizNewMatIds_GLOBAL_R(:)
    integer RECL_FizMatCounter_GLOBAL_R !Общее количество добавленных материалов перегружаемых на этом шаге (ГЛОБАЛЬНАЯ ПЕРЕМЕННАЯ)

    !ДЛЯ ПРОВЕРКИ ОШИБОК
    integer SizeOfGroupRecl, SizeOfTypeRecl, SizeOfNkRecl !размерность входных массивов REPRORYV
    integer  RECL_ERR_FizZone1, RECL_ERR_FizZone2 !количество выгружаемых физических зон (должны быть одинаковыми)
    real RECL_ERR_volume1, RECL_ERR_volume2 !выгружаемые объемы ТВСок на соседних шагах (должны быть равными)

    !Время до переработки
    real :: T1Recl = 0.0

    !текущий каталог пользователя
    character*180 mydir


    !идентификатор для открытия файла
    integer unt1,unt2,unt3,unt4
    data unt1,unt2,unt3,unt4 /991,992,993,994/
    !входной и выходной файл JARFR
    character*60 :: inp1 = 'inp.dat', inp_temp = 'inp_temp.dat', out1 = 'out.rez', inp_new = 'inp_new.dat', &
    inpRECL = 'inp_RECYCLE.dat', inpRECL_temp = 'inpRECYCLE_temp.dat', inp_new_to_run='inp_new_to_run.dat'

    character*60 :: DelComm = 'erase /F /S /Q /A';
end module CommonModJARFR
