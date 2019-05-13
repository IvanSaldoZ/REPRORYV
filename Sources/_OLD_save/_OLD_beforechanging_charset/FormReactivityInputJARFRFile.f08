!ФОРМИРУЕМ НОВЫЙ ВХОДНОЙ ФАЙЛ ДЛЯ JARFR ДЛЯ АНАЛИЗА ЭФФЕКТОВ РЕАКТИВНОСТИ
subroutine FormReactivityInputJARFRFile
    use CommonModJARFR
    !копируем в файл inp_new_to_run_Reactivity, чтобы потом его запустить
    command = 'copy '//trim(inp_new_to_run)//' inp_new_to_run_Reactivity.dat 1>>tmp 2>&1'
    re_i = system(command)
    open(unt1, file = 'inp_new_to_run_Reactivity.dat', status = 'OLD')
    if (DoplerOn.EQ.1) then
      !меняем температуру для делящихся на величину повышения температуры из файла REPRORYV
      do i=1,Nf
        Tem(i) = Tem(i) + DoplerDelta
      end do
    end if !if (DoplerOn.EQ.1)
    if (DensKoeffOn.EQ.1) then
      !меняем плотности в соответствии с входным файлом RERPRYV и массивом из него DensChange
      k = 1
      do i=1,M1
        do j=1,Ner1
          !ADx2(i,j) = Ad(k) !заполняем двухмерный массив концентрациями
          Ad(k) = Ad(k)*DensChange(j)
          k = k + 1
        end do
      end do
    end if !if (DensKoeffOn.EQ.1)

    write(unt1, NML = Mim)
    write(unt1, NML = D26)
    write(unt1, NML = Dan)
    write(unt1, NML = Obr)
    write(unt1, NML = Upbn)
    close(unt1)
    !копируем, если у нас включен расчёт
    command = 'copy inp_new_to_run_Reactivity.dat '// &
    & 'recycle\'//trim(StepGlobal_STR)//'stp\'//trim(StepGlobal_STR)//'stp_jarfr_input_Reactivity.txt 1>>tmp 2>&1'
    re_i = system(command)



end subroutine
