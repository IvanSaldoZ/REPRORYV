module ConvertCyr
integer, private, parameter:: StrLen=9999

contains

function DosToWin(String)
character(len=StrLen):: DosToWin
character(len=*), intent(in):: String
integer, dimension(128:255):: DW
data DW / &
z'C0', z'C1', z'C2', z'C3', z'C4', z'C5', z'C6', z'C7', z'C8', z'C9', z'CA', z'CB', z'CC', z'CD', z'CE', z'CF',  &
z'D0', z'D1', z'D2', z'D3', z'D4', z'D5', z'D6', z'D7', z'D8', z'D9', z'DA', z'DB', z'DC', z'DD', z'DE', z'DF',  &
z'E0', z'E1', z'E2', z'E3', z'E4', z'E5', z'E6', z'E7', z'E8', z'E9', z'EA', z'EB', z'EC', z'ED', z'EE', z'EF',  &
z'85', z'85', z'85', z'A6', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85',  &
z'85', z'85', z'85', z'85', z'85', z'97', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85',  &
z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85', z'85',  &
z'F0', z'F1', z'F2', z'F3', z'F4', z'F5', z'F6', z'F7', z'F8', z'F9', z'FA', z'FB', z'FC', z'FD', z'FE', z'FF',  &
z'A8', z'B8', z'AA', z'BA', z'AF', z'BF', z'A1', z'A2', z'B0', z'95', z'B7', z'B1', z'B9', z'A4', z'98', z'A0'/
character(len=1) Sym
integer i, k, index
  DosToWin=String
  do i=1, len_trim(String)
    Sym=DosToWin(i:i); index=ichar(Sym)
      if (index >= 128) then
        k=DW(index); Sym=char(k)
        DosToWin(i:i)=Sym
      end if
  end do
end function DosToWin

function WinToDos(String)
character(len=StrLen):: WinToDos
character(len=*), intent(in):: String
integer, dimension(128:255):: WD
data WD / &
z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0',  &
z'B0', z'B0', z'B0', z'B0', z'B0', z'F9', z'B0', z'C5', z'FE', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0', z'B0',  &
z'FF', z'F6', z'F7', z'B0', z'FD', z'B0', z'B3', z'15', z'F0', z'B0', z'F2', z'B0', z'B0', z'B0', z'B0', z'F4',  &
z'F8', z'FB', z'B0', z'B0', z'B0', z'B0', z'B0', z'FA', z'F1', z'FC', z'F3', z'B0', z'B0', z'B0', z'B0', z'F5',  &
z'80', z'81', z'82', z'83', z'84', z'85', z'86', z'87', z'88', z'89', z'8A', z'8B', z'8C', z'8D', z'8E', z'8F',  &
z'90', z'91', z'92', z'93', z'94', z'95', z'96', z'97', z'98', z'99', z'9A', z'9B', z'9C', z'9D', z'9E', z'9F',  &
z'A0', z'A1', z'A2', z'A3', z'A4', z'A5', z'A6', z'A7', z'A8', z'A9', z'AA', z'AB', z'AC', z'AD', z'AE', z'AF',  &
z'E0', z'E1', z'E2', z'E3', z'E4', z'E5', z'E6', z'E7', z'E8', z'E9', z'EA', z'EB', z'EC', z'ED', z'EE', z'EF'/
character(len=1) Sym
integer i, k, index
  WinToDos=String
  do i=1, len_trim(String)
    Sym=WinToDos(i:i); index=ichar(Sym)
      if (index >= 128) then
        k=WD(index); Sym=char(k)
        WinToDos(i:i)=Sym
     end if
  end do
end function WinToDos

end module ConvertCyr
