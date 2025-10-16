program adasre 
    use variables
    use configs
    use reference_data
    implicit none 

    call init 
    call input
    call postProcess
    call adf04 
    call deinit 
    
end program