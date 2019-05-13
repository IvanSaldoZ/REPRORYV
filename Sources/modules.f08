module CommonModJARFR
    character*6 :: CONST_VERSION = '0.25.1' !������ ���������
    character*10 :: CONST_DATE = '2017.01.04' !���� ������� 0.25

    real :: CONST_NA = 6.022E23  ! ����� �������� [1/����]
    real :: CONST_barn = 1E24    ! ������� ��2 � ����� ����� [��^2]

    !��� ������ �� �������� (������� ������� ���, ����� ���������� �����/������������)
    type TNuclDataCommon
      integer         nucl_order_num    !���������� ����� � �������
      character*4     nucl_Caption      !�������� ������� �� ����
      integer         nucl_numberID     !����� ������� �� ����
      real            nucl_atomic_mass  !������� ���
    end type
    type(TNuclDataCommon), allocatable :: NuclDataCommon(:)

    !������ �� ��������� ������� Pu � U
    type TMassData
      SEQUENCE
      real            mass_loaded         !����������� ����� � ��
      real            mass_before         !����� ����� � �� �� ������
      real            mass_after          !����� ����� � �� ����� ������
      real            mass_unloaded       !����������� ����� �� ��
      real            mass_recycle_before !����� ����� ����� ������������
      real            mass_recycle_after  !����� ����� ����� ����������� (������ ��� ������ ���� ��������� � �������� ����) - ����������� ����������
    end type

    !������ �� ������� �������� Pu
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

    type(TMassData), allocatable :: MassDataU(:)       !�����  �����
    type(TMassData), allocatable :: MassDataPu(:)      !�����  ��������
    type(TPuVector), allocatable :: PuVector(:)        !������ �������� �����        �������
    type(TUVector),  allocatable :: UVector(:)         !������ �����    �����        �������
    type(TPuVector), allocatable :: PuVector_LOADED(:) !������ �������� ������������ �������
    type(TUVector),  allocatable :: UVector_LOADED(:)  !������ �����    ������������ �������



    integer       :: isTEST = 3          !<Show debug info? (for developers only))
    integer       :: re_i                !<I for showing system information
    integer       :: maxNk,NkDimention   !<������������ ����� � ����������� Nk (����� ��� ����������� ������� ������� KT � �����)
    integer       :: MaxNSLZ             !������������ ����� � NSLZ - �����, ����� ���������� ����������� ������� KT �� �����
    integer       :: maxNkRECL           !������������ ���, �� ������� ���������� ���������� (�� ����� ��������� REPRORY)
    integer       :: StepGlobal          !������� �������� ����, �� ������� ��������� ���������
    character*255 :: command             !���� ����� �������� ������-������� ��� ���������� ����� ��������� ������
    character*4   :: StepGlobal_STR      !������ �������� ���� �� ���������
    character*250 :: CurDir              !����� �������� ����
    integer       :: MaxGroupRecl        !������������ �������� ����� �������� ��� ���������� �� �������� ����� REPRORYV�
    integer       :: MkCampCounter=1     !������� ������� ������������ (������������ ����� ��� ��� ��������� ��������)


    !MIM
    NAMELIST /Mim/ Zsig,m1,m1v,Jpfr,Jdpa,Ner1,Nred,Sostav,Nzon,Ad,Tem,Mng,Mnog,Ncol
    !���������� �� Mim
    integer  Zsig,M1,M1v,Jdpa,Ner1,Nred,Mng,Mnog
    integer, allocatable                         :: ncol(:),nzon(:),sostav(:)
    real,    allocatable                         :: Ad(:),                    Tem(:)

    !D26
    NAMELIST /D26/ Ndm,Nt,Nf,Nr2,Nge,Nr1,Nlg,Jitr,H,Nslz,KT,Ns,Nk,Nsm,Hzr,gam,gm,JDRO
    !���������� �� D26
    integer  Ndm,Nt,Nf,Nr2,Nge,Nr1,Nlg,Jitr,JDRO
    real     H,gam(1:1300),gm(1:52)
    integer, allocatable                         :: Nslz(:),KT(:),Ns(:),Nk(:),Nsm(:)
    real,    allocatable                         :: Hzr(:)

    !DAN
    NAMELIST /DAN/ Ng,Mg,Nt,Nf,Pw,Juch,J2x,MAXI,Jitr,Eps1,Eps2,Eps3,Nnaz,Ncaz,Jtv, &
    Jupbn,Jfiz,Jnum,Jpri,Ipr,Jprw,JJAP
    !���������� �� Dan
    integer Ng,Mg,Juch,J2x,MAXI,Nnaz,Ncaz,Jtv,Jupbn,Jfiz,Jnum,Jpri,Ipr,JJAP
    real Pw,Eps1,Eps2,Eps3
    integer, allocatable :: Jprw(:)

    !OBR
    NAMELIST /OBR/ Naz,Ntor,Nbok,Ngel,Nvel,Ngor,Nvos,Nkv,Ntnz,Jpfz,nkr
    !���������� �� Obr
    integer Naz,Ntor,Nbok,Ngel,Nvel,Nkv,nkr
    integer, allocatable :: Ngor(:),Nvos(:),Ntnz(:),Jpfz(:)

    !Upbn
    NAMELIST /UPBN/ Burn_b10,T,Ndt,T_Usr,PWM,Jprn,Jdpas,Jdpt,Ipr,Jprw
    !���������� �� Obr
    integer Burn_b10,Ndt,Jdpas
    integer, allocatable :: Jprn(:),Jdpt(:)
    real T
    real, allocatable    :: T_Usr(:),PWM(:)

    !� ������ ��� ���� ������� ���� REPRORYV
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
    !�������� ����������� Keff - ��������� ������ ����� ���� � ������� �� Keff, ����� �������, ��� ������� �����������
    real    :: CritEpsilon=0.01
    !���, � �������� ����� ��������� JARFR
    integer :: StartStep=1
    !���, � �������� ����������� JARFR ��� �������� �������� ������������
    integer :: StartStepReactivity=1
    !����� �� ��������� �������������� ������ ��������� ���������� ������� �� bpsd {1 - ��, 0 - ���}
    integer :: BpsdOn=0
    !����� ��������� ���������� BPSD.
    !    0 - ������ ���������� ������� �� BPSD,
    !    1 - ������ �������� ������� ������: ������� ��������� + ������
    !    2 - ������ ������� ������� ������: ������� ���������� - ������.
    integer :: BpsdMode=0
    ! ������� ������ ������� �� ������ ���� {1 - ��������, 0 - �� ����������� ������ ������-�������}
    integer :: DoplerOn = 0
    ! �� ������� �������� ��������� ����������� �������, ����� ��������� ������ �������,
    ! ��������, ���� DoplerDelta = 300, �� ��� ����������� T=1200K, ����������� ����������� ����� �������� ��� T=1500K
    real    :: DoplerDelta = 300.0
    !������� ������������ ������� ������������ �� ������ ���� {1 - ��������, 0 - �� ����������� ���������� ������ ������������}
    integer :: DensKoeffOn = 0

    !������ �� �����, ����������� ��� ������� ����������
    !��� ���������� ���� (� ������������ � Nzon)
    !0 - ���� ���������, ������������ ������ (Disabled) �� ���� ����, ��� ��� ����� �����������
    !1 - �� ����������� �� ��, ������� ���� ��� ���������-����������� �������
    !2 - ����, ������� ����� ������������ �� ���� ����
    !3 - ��������������� ��������� (����, ������� ������ �� ������������� �������)
    !4 - ����� ���� ��� ������� ����
    !5 - �������������� ���� ����� �����������, ����� ������������ ��� ������ �������
    integer, allocatable  :: RECL_nzon_type(:), RECL_nzon_type_temp(:)

    real, allocatable     :: IsotReclConc(:) !<Concentrations of each isotopes
    integer, allocatable  :: RECL_FizStepLoaded(:)  !<step on which the zone is loaded

    !<������ �������� (���/���� - 1/0) ���. 0 �������� ��� ���, ������� ��� ������ �� ������������ (����� �� ������������ ��� ����� �������� �������������� ���)
    integer, allocatable  :: RECL_TVSnum_Enabled(:)
    !������ �������� (���/���� - 1/0) ���-���. 0 �������� ��� ��� ���-���, ������� ���� ��� �� ������������ (�� �� ����� ������������ ��� ����� �������� �������������� ���)
    !integer, allocatable :: RECL_Fiz_Enabled(:)
    !������ ������������ ����� � ������ ���������� ��� ��� ���, ������� ���������� ������ ����������� ����� (����� ��� ������ �������� ������������ - � ������ 1-�� ����)  (���������� ����������)
    integer, allocatable  :: RECL_FizOldMatIds_GLOBAL(:), RECL_FizNewMatIds_GLOBAL(:)
    integer RECL_FizMatCounter_GLOBAL !����� ���������� ����������� ���������� ����������� �� ���������� ����  (���������� ����������)
    !������ ������������ ����� � ������ ���������� ��� ��� ������������� ����� (����� ��� ������ �������� ������������ - � ������ 1-�� ����)  (���������� ����������)
    integer, allocatable  :: RECL_FizOldMatIds_GLOBAL_R(:), RECL_FizNewMatIds_GLOBAL_R(:)
    integer RECL_FizMatCounter_GLOBAL_R !����� ���������� ����������� ���������� ������������� �� ���� ���� (���������� ����������)

    !��� �������� ������
    integer SizeOfGroupRecl, SizeOfTypeRecl, SizeOfNkRecl !����������� ������� �������� REPRORYV
    integer  RECL_ERR_FizZone1, RECL_ERR_FizZone2 !���������� ����������� ���������� ��� (������ ���� �����������)
    real RECL_ERR_volume1, RECL_ERR_volume2 !����������� ������ ����� �� �������� ����� (������ ���� �������)

    !����� �� �����������
    real :: T1Recl = 0.0

    !������� ������� ������������
    character*180 mydir


    !������������� ��� �������� �����
    integer unt1,unt2,unt3,unt4
    data unt1,unt2,unt3,unt4 /991,992,993,994/
    !������� � �������� ���� JARFR
    character*60 :: inp1 = 'inp.dat', inp_temp = 'inp_temp.dat', out1 = 'out.rez', inp_new = 'inp_new.dat', &
    inpRECL = 'inp_RECYCLE.dat', inpRECL_temp = 'inpRECYCLE_temp.dat', inp_new_to_run='inp_new_to_run.dat'

    character*60 :: DelComm = 'erase /F /S /Q /A';
end module CommonModJARFR
