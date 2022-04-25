namespace Elm.Time


open Elm.Core


/// A computer representation of time. It is the same all over Earth, so if we
/// have a phone call or meeting at a certain POSIX time, there is no ambiguity.
/// 
/// It is very hard for humans to _read_ a POSIX time though, so we use functions
/// like [`toHour`](#toHour) and [`toMinute`](#toMinute) to `view` them.
[<Struct>]
type Posix = 
    private
    | MsSinceEpoch of int


(* WEEKDAYS AND MONTHS *)


/// Represents a `Weekday` so that you can convert it to a `string` or `int`
/// however you please. For example, if you need the Japanese representation, you
/// can say:
/// 
///     let toJapaneseWeekday weekday =
///         match weekday with
///         | Mon -> "月"
///         | Tue -> "火"
///         | Wed -> "水"
///         | Thu -> "木"
///         | Fri -> "金"
///         | Sat -> "土"
///         | Sun -> "日"
/// 
type Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun


/// Represents a `Month` so that you can convert it to a `string` or `int`
/// however you please. For example, if you need the Danish representation, you
/// can say:
/// 
///     let toDanishMonth month =
///         match month with
///         | Jan -> "januar"
///         | Feb -> "februar"
///         | Mar -> "marts"
///         | Apr -> "april"
///         | May -> "maj"
///         | Jun -> "juni"
///         | Jul -> "juli"
///         | Aug -> "august"
///         | Sep -> "september"
///         | Oct -> "oktober"
///         | Nov -> "november"
///         | Dec -> "december"
type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec


/// Currently the public API only needs:
/// - `start` is the beginning of this `Era` in "minutes since the Unix Epoch"
/// - `offset` is the UTC offset of this `Era` in minutes
/// 
/// But eventually, it will make sense to have `abbr : String` for `PST` vs `PDT`
type Era = 
    {
        Start:int
        Offset:int 
    }


/// Information about a particular time zone.
/// 
/// The [IANA Time Zone Database][iana] tracks things like UTC offsets and
/// daylight-saving rules so that you can turn a `Posix` time into local times
/// within a time zone.
/// 
/// See [`utc`](#utc) and [`here`](#here), to learn how to obtain `Zone` values.
/// [iana]: https://www.iana.org/time-zones
type Zone =
    private
    | Zone of minutes:int * Era list


[<RequireQualifiedAccess>]
module Time = 
    open System 

    /// Get the POSIX time at the moment.
    let now() = 
        DateTimeOffset.Now.ToUnixTimeMilliseconds()
        |> int
        |> MsSinceEpoch


    /// Turn a `Posix` time into the number of milliseconds since 1970 January 1
    /// at 00:00:00 UTC. It was a Thursday.
    let posixToMillis (MsSinceEpoch millis) = 
        millis


    /// Turn milliseconds into a `Posix` time.
    let millisToPosix millis = 
        MsSinceEpoch millis

    
    (* .Net Interop *)

    let toDateTimeOffset(MsSinceEpoch t) = 
        DateTimeOffset.FromUnixTimeMilliseconds(int64 t)


    (* TIME ZONES *)


    /// The time zone for Coordinated Universal Time ([UTC][])
    /// The `utc` zone has no time adjustments. It never observes daylight-saving
    /// time and it never shifts around based on political restructuring.
    /// [UTC]: https://en.wikipedia.org/wiki/Coordinated_Universal_Time
    let utc : Zone =
        Zone(0, [])


    /// Produce a `Zone` based on the current UTC offset.
    /// 
    /// **Accuracy Note:** This function can only give time zones like `Etc/GMT+9` or
    /// `Etc/GMT-6`. It cannot give you `Europe/Stockholm`, `Asia/Tokyo`, or any other
    /// normal time zone from the [full list][tz] due to limitations in JavaScript.
    /// For example, if you run `here` in New York City, the resulting `Zone` will
    /// never be `America/New_York`. Instead you get `Etc/GMT-5` or `Etc/GMT-4`
    /// depending on Daylight Saving Time. So even though browsers must have internal
    /// access to `America/New_York` to figure out that offset, there is no public API
    /// to get the full information. This means the `Zone` you get from this function
    /// will act weird if (1) an application stays open across a Daylight Saving Time
    /// boundary or (2) you try to use it on historical data.
    /// 
    /// **Future Note:** We can improve `here` when there is good browser support for
    /// JavaScript functions that (1) expose the IANA time zone database and (2) let
    /// you ask the time zone of the computer. The committee that reviews additions to
    /// JavaScript is called TC39, and I encourage you to push for these capabilities! I
    /// cannot do it myself unfortunately.
    let here : Zone = 
        Zone(int DateTimeOffset.Now.Offset.TotalMinutes, [])


    (* DATE HELPERS *)

    let private flooredDiv (numerator:int) (denominator:float) : int =
        floor (toFloat numerator / denominator)


    type private CivilDate = 
        {
            Year: int
            Month: int
            Day: int
        }

    let private toCivil (minutes:int) : CivilDate = 
        let rawDay    = flooredDiv minutes (60.0 * 24.0) + 719468
        let era       = (if rawDay >= 0 then rawDay else rawDay - 146096) / 146097
        let dayOfEra  = rawDay - era * 146097 //-- [0, 146096]
        let yearOfEra = (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 - dayOfEra / 146096) / 365  //-- [0, 399]
        let year      = yearOfEra + era * 400
        let dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100) //-- [0, 365]
        let mp        = (5 * dayOfYear + 2) / 153 //-- [0, 11]
        let month     = mp + (if mp < 10 then 3 else -9) //-- [1, 12]

        {
            Year = year + (if month <= 2 then 1 else 0)
            Month = month
            Day = dayOfYear - (153 * mp + 2) / 5 + 1 //-- [1, 31]
        }

    let private toAdjustedMinutes (Zone(defaultOffset, eras)) time =
        let rec toAdjustedMinutesHelp defaultOffset posixMinutes eras =
            match eras with
            | [] ->
                posixMinutes + defaultOffset

            | era :: olderEras ->
            if era.Start < posixMinutes then
                posixMinutes + era.Offset
            else
                toAdjustedMinutesHelp defaultOffset posixMinutes olderEras
        
        toAdjustedMinutesHelp defaultOffset (flooredDiv (posixToMillis time) 60000) eras


    (* DATES *)

    /// What year is it?!
    /// 
    /// `toYear utc (millisToPosix 0) = 1970`
    let toYear zone time =
        (toCivil (toAdjustedMinutes zone time)).Year


    /// What month is it?!
    /// 
    /// `toMonth utc (millisToPosix 0) = Jan`
    let toMonth zone time =
        match (toCivil (toAdjustedMinutes zone time)).Month with
        | 1  -> Jan
        | 2  -> Feb
        | 3  -> Mar
        | 4  -> Apr
        | 5  -> May
        | 6  -> Jun
        | 7  -> Jul
        | 8  -> Aug
        | 9  -> Sep
        | 10 -> Oct
        | 11 -> Nov
        | _  -> Dec


    /// What day is it?! (Days go from 1 to 31)
    /// 
    /// `toDay utc (millisToPosix 0) = 1`
    let toDay zone time =
        (toCivil (toAdjustedMinutes zone time)).Day


    /// What day of the week is it?
    /// 
    /// `toWeekday utc (millisToPosix 0) = Thu`
    let toWeekday zone time =
        match modBy 7 (flooredDiv (toAdjustedMinutes zone time) (60.0 * 24.0)) with
        | 0 -> Thu
        | 1 -> Fri
        | 2 -> Sat
        | 3 -> Sun
        | 4 -> Mon
        | 5 -> Tue
        | _ -> Wed


    /// What hour is it? (From 0 to 23)
    /// 
    /// `toHour utc (millisToPosix 0) = 0 // 12am`
    let toHour zone time =
        modBy 24 (flooredDiv (toAdjustedMinutes zone time) 60)


    /// What minute is it? (From 0 to 59)
    /// 
    /// `toMinute utc (millisToPosix 0) = 0`
    /// 
    /// This can be different in different time zones. Some time zones are offset
    /// by 30 or 45 minutes!
    let toMinute zone time =
        modBy 60 (toAdjustedMinutes zone time)

    
    /// What second is it?
    /// 
    /// `toSecond utc (millisToPosix    0) = 0`
    /// `toSecond utc (millisToPosix 1234) = 1`
    /// `toSecond utc (millisToPosix 5678) = 5`
    ///
    let toSecond (_:Zone) time =
        modBy 60 (flooredDiv (posixToMillis time) 1000.0)


    /// What millisecond is it?
    /// 
    /// `toMillis utc (millisToPosix    0) = 0`
    /// `toMillis utc (millisToPosix 1234) = 234`
    /// `toMillis utc (millisToPosix 5678) = 678`
    ///
    let toMillis (_:Zone) time =
        modBy 1000 (posixToMillis time)


    (* FOR PACKAGE AUTHORS *)

    /// **Intended for package authors.**
    /// 
    /// The documentation of [`here`](#here) explains that it has certain accuracy
    /// limitations that block on adding new APIs to JavaScript. The `customZone`
    /// function is a stopgap that takes:
    /// 
    /// 1. A default offset in minutes. So `Etc/GMT-5` is `customZone (-5 * 60) []`
    /// and `Etc/GMT+9` is `customZone (9 * 60) []`.
    /// 2. A list of exceptions containing their `start` time in "minutes since the Unix
    /// epoch" and their `offset` in "minutes from UTC"
    /// 
    /// Human times will be based on the nearest `start`, falling back on the default
    /// offset if the time is older than all of the exceptions.
    /// 
    /// When paired with `getZoneName`, this allows you to load the real IANA time zone
    /// database however you want: HTTP, cache, hardcode, etc.
    ///
    /// **Note:** If you use this, please share your work in an Elm community forum!
    /// I am sure others would like to hear about it, and more experience reports will
    /// help me and the any potential TC39 proposal.
    let customZone minutes exceptions =
        Zone(minutes, exceptions)