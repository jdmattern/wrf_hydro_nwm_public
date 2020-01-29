! This module defines and instantiates objects
! for an rfc forecasts type reservoir's
! parameters/properties. Properties holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.

module module_rfc_forecasts_properties
    use module_reservoir_utilities, only: read_netcdf_lake_id, &
                                          read_reservoir_parameters_netcdf_rfc_gage_id, &
                                          read_reservoir_parameters_netcdf_rfc_integer, &
                                          handle_err
    use module_reservoir, only: reservoir_properties
    use netcdf

    implicit none

    ! Extend/derive rfc forecasts properties from the abstract base
    ! type for reservoir properties.
    type, extends(reservoir_properties) :: rfc_forecasts_properties_interface
        integer             :: lake_number         ! lake number
        character(len=5)    :: rfc_gage_id
        integer             :: lookback_seconds
        integer             :: total_counts
        integer             :: observed_counts
        integer             :: time_step_seconds
        integer             :: rfc_forecast_persist_days
        integer             :: rfc_forecast_persist_seconds
        integer             :: rfc_timeslice_offset_hours
        integer             :: observation_hours
        character(len=19)   :: start_date
        character(len=256)  :: time_series_path
        integer             :: forecast_lookback_hours

    contains

        procedure :: init => rfc_forecasts_properties_init
        procedure :: destroy => rfc_forecasts_properties_destroy

    end type rfc_forecasts_properties_interface

    integer, parameter :: seconds_in_day = 86400
    integer, parameter :: observation_hours = 48

contains

    ! RFC Forecasts Properties Constructor
    subroutine rfc_forecasts_properties_init(this, lake_number, rfc_forecasts_parameter_file, start_date, &
        time_series_path, forecast_lookback_hours)
        implicit none
        class(rfc_forecasts_properties_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in)             :: lake_number                   ! lake number
        character(len=*), intent(in)    :: rfc_forecasts_parameter_file
        character(len=19),  intent(in)  :: start_date
        character(len=256), intent(in)  :: time_series_path
        integer,            intent(in)  :: forecast_lookback_hours
        integer                         :: ncid, var_id, lake_id_index
        integer                         :: status                        ! status of reading NetCDF

        this%lake_number = lake_number

        this%observation_hours = observation_hours
        this%start_date = start_date

        this%time_series_path = time_series_path

        this%forecast_lookback_hours = forecast_lookback_hours
print *, 'start reading reservoir param file for reservoir ', lake_number

        ! Open RFC Reservoir Parameter NetCDF file
        status = nf90_open(path = rfc_forecasts_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open rfc forecasts parameter file")

print *, '1 reading reservoir param file for reservoir ', lake_number


        ! Read relevant properties from Persistence Parameter NetCDF
        call read_netcdf_lake_id(ncid, lake_number, "rfc_lake_id", lake_id_index)

print *, '2 reading reservoir param file for reservoir ', lake_number


        call read_reservoir_parameters_netcdf_rfc_gage_id(ncid, lake_id_index, "rfc_gage_id", this%rfc_gage_id)

print *, '3 reading reservoir param file for reservoir ', lake_number


        call read_reservoir_parameters_netcdf_rfc_integer(ncid, lake_id_index, "rfc_forecast_persist", this%rfc_forecast_persist_days)

print *, '4 reading reservoir param file for reservoir ', lake_number


        this%rfc_forecast_persist_seconds = this%rfc_forecast_persist_days * seconds_in_day

        call read_reservoir_parameters_netcdf_rfc_integer(ncid, lake_id_index, "rfc_timeslice_offset", this%rfc_timeslice_offset_hours)

print *, 'Done reading reservoir param file for reservoir ', lake_number


    end subroutine rfc_forecasts_properties_init

    ! RFC Forecasts Properties Destructor
    subroutine rfc_forecasts_properties_destroy(this)
        implicit none
        class(rfc_forecasts_properties_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine rfc_forecasts_properties_destroy

end module module_rfc_forecasts_properties
