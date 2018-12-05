namespace Shared

type DataPoint =
    {x : float
     y : float}

type DataUrl =
    {Url : string}

type Msg<'a> =
    {Type : string
     Data : 'a}
