module VecI
open System

type VecI(x : int, y : int) =
    static member Zero = VecI(0, 0)
    member this.X = x
    member this.Y = y

    override this.Equals other =
        match other with
        | :? VecI as other -> other.X = this.X && other.Y = this.Y
        | _ -> false

    override this.ToString() = "[" + x.ToString() + "; " + y.ToString() + "]"

    override this.GetHashCode() = (x, y).GetHashCode()

    interface IComparable<VecI> with
        member this.CompareTo(other: VecI) = 
            compare (this.X, this.Y) (other.X, other.Y)

    interface IComparable with
        member this.CompareTo(obj : System.Object) =
            match obj with
            | null -> 1
            | :? VecI as other -> (this :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a Vector"

type VecI with
    static member map (fn : int -> int) (v: VecI) = VecI(fn v.X, fn v.Y)
    static member (+) (a : VecI, b : VecI) = VecI(a.X + b.X, a.Y + b.Y)
    static member (-) (a : VecI, b : VecI) = VecI(a.X - b.X, a.Y - b.Y)
    static member (*) (v : VecI, a) = v |> VecI.map ((*) a)
    static member (*) (a, v: VecI) = v |> VecI.map ((*) a)
    static member length (v: VecI) = (v.X * v.X + v.Y * v.Y) |> float |> sqrt
    static member toPair (this : VecI) = this.X, this.Y
