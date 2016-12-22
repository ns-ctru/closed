drop_spurious <- function(df             = df4,
                          .indicator     = indicator,
                          .sub.indicator = sub.indicator){
    if(.indicator == 'ed attendances' & .sub.indicator == 'any'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ed attendances' & .sub.indicator == 'ambulance'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'unnecessary ed attendances' & .sub.indicator == 'all'){
        df <- dplyr::filter(df, town != 'Hartlepool')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to dest'){
        df <- dplyr::filter(df, town != 'White Haven')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to dest'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to dest'){
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to scene any'){
        df <- dplyr::filter(df, town != 'White Haven')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to scene any'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'call to scene any'){
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'call to scene any'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'call to scene any'){
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to scene conveying'){
        df <- dplyr::filter(df, town != 'White Haven')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to scene conveying'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to scene conveying'){
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'call to scene conveying'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'call to scene conveying'){
        df <- dplyr::filter(df, town != 'Rochdale')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'scene to dest'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'scene to dest'){
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'scene to dest'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'scene to dest'){
        df <- dplyr::filter(df, town != 'Rochdale')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'dest to clear'){
        df <- dplyr::filter(df, town != 'White Haven')
    }
    if(.indicator == 'ambulance mean times' & .sub.indicator == 'dest to clear'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'dest to clear'){
        df <- dplyr::filter(df, town != 'Hemel Hempstead')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'dest to clear'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator== 'ambulance mean times' & .sub.indicator == 'dest to clear'){
        df <- dplyr::filter(df, town != 'Rochdale')
    }
    if(.indicator == 'ambulance green calls' & .sub.indicator == 'green calls'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ambulance green calls' & .sub.indicator == 'green calls'){
        df <- dplyr::filter(df, town != 'Warwick')
    }
    if(.indicator == 'ambulance green calls' & .sub.indicator == 'green calls'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator == 'ambulance green calls' & .sub.indicator == 'fraction not conveyed'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ambulance green calls' & .sub.indicator == 'fraction not conveyed'){
        df <- dplyr::filter(df, town != 'Warwick')
    }
    if(.indicator == 'ambulance green calls' & .sub.indicator == 'fraction not conveyed'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator == 'ambulance red calls' & .sub.indicator == 'hospital transfers'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ambulance red calls' & .sub.indicator == 'hospital transfers'){
        df <- dplyr::filter(df, town != 'Warwick')
    }
    if(.indicator == 'ambulance red calls' & .sub.indicator == 'hospital transfers'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    if(.indicator == 'ambulance red calls' & .sub.indicator == 'total'){
        df <- dplyr::filter(df, town != 'Grimsby')
    }
    if(.indicator == 'ambulance red calls' & .sub.indicator == 'total'){
        df <- dplyr::filter(df, town != 'Warwick')
    }
    if(.indicator == 'ambulance red calls' & .sub.indicator == 'total'){
        df <- dplyr::filter(df, town != 'Newark')
    }
    return(df)
}
