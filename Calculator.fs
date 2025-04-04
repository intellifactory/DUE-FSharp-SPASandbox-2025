namespace MySPA01

open WebSharper

[<JavaScript>]
module Calculator =
    module Ast =
        type var = string

        type Expr =
            | Number   of float
            | BinOp    of (float -> float -> float) * Expr * Expr
            | Var      of var
            | FunCall  of var * Expr

            static member Sum (e1, e2)   = BinOp ((+), e1, e2)
            static member Diff (e1, e2)  = BinOp ((-), e1, e2)
            static member Prod (e1, e2)  = BinOp (( *), e1, e2)
            static member Ratio (e1, e2) = BinOp ((/), e1, e2)

    module Language =
        open System
        open WebSharper.JavaScript

        let private matchToken pattern (s: string) : (string * string) option =
            let regexp = new RegExp("^(" + pattern + ")(.*)")
            if regexp.Test s then
                let results = regexp.Exec s
                if results ==. null then
                    None
                else
                    Some results
                |> Option.map (fun x -> (x.[1], x.[2]))
            else
                None

        // eta reduction
        let (|WHITESPACE|_|) s = matchToken @"[ |\t|\n|\n\r]+" s

        let rec MatchTokenNoWS s pattern =
            match (|WHITESPACE|_|) s with
            | Some (_, rest) ->
                rest |> matchToken pattern
            | None ->
                s |> matchToken pattern

        let MatchToken s f pattern =
            pattern |> MatchTokenNoWS s |> Option.bind f

        let MatchSymbol s pattern =
            pattern |> MatchToken s (fun (_, rest) -> rest |> Some)

        let rec (|Star|_|) f acc s =
            match f s with
            | Some (res, rest) ->
                (|Star|_|) f (res :: acc) rest
            | None ->
                (acc |> List.rev , s) |> Some

        let (|NUMBER|_|) s =
            @"[0-9]+\.?[0-9]*" |> MatchToken s
                (fun (n, rest) -> (n |> Double.Parse, rest) |> Some)

        let (|ID|_|) s =
            "[a-zA-Z]+" |> MatchToken s (fun res -> res |> Some)

        let (|PLUS|_|)   s = @"\+" |> MatchSymbol s
        let (|MINUS|_|)  s = @"\-"  |> MatchSymbol s
        let (|MUL|_|)    s = @"\*" |> MatchSymbol s
        let (|DIV|_|)    s = "/"  |> MatchSymbol s
        let (|LPAREN|_|) s = @"\(" |> MatchSymbol s
        let (|RPAREN|_|) s = @"\)" |> MatchSymbol s

        // Non-terminal symbols
        // Productions
        let rec (|Factor|_|) = function
            | NUMBER (n, rest) ->
                (Ast.Expr.Number n, rest) |> Some
            | ID (v, rest) ->
                match rest with
                | LPAREN (Expression (arg, RPAREN rest)) ->
                    (Ast.Expr.FunCall (v, arg), rest) |> Some
                | _ ->
                    (Ast.Expr.Var v, rest) |> Some
            | LPAREN (Expression (e, RPAREN rest)) ->
                    (e, rest) |> Some
            | _ ->
                None

        and (|Term|_|) = function
            | Factor (e1, rest) ->
                match rest with
                | MUL (Term (e2, rest)) ->
                    (Ast.Expr.Prod (e1, e2), rest) |> Some
                | DIV (Term (e2, rest)) ->
                    (Ast.Expr.Ratio (e1, e2), rest) |> Some
                | _ ->
                    (e1, rest) |> Some
            | _ ->
                None

        and (|Sum|_|) = function
            | Term (e1, rest) ->
                match rest with
                | PLUS (Sum (e2, rest)) ->
                    (Ast.Expr.Sum (e1, e2), rest) |> Some
                | MINUS (Sum (e2, rest)) ->
                    (Ast.Expr.Diff (e1, e2), rest) |> Some
                | _ ->
                    (e1, rest) |> Some
            | _ ->
                None

        and (|Expression|_|) = (|Sum|_|)

        let (|Eof|_|) s =
            if String.IsNullOrEmpty s then
                () |> Some
            else
                match s with
                | WHITESPACE (_, rest) when rest |> String.IsNullOrEmpty ->
                    () |> Some
                | _ ->
                    None

    module Evaluator =
        open Ast

        let rec Eval (env: (string * float) list) e =
            match e with
            | Expr.Number num        -> num
            | Expr.BinOp (f, e1, e2) -> f (Eval env e1) (Eval env e2)
            | Expr.Var v             ->
                env
                |> List.tryFind (fun (_v, _) -> _v = v)
                |> function
                    | None ->
                        "Unbound variable: " + v |> failwith
                    | Some (_, value) ->
                        value
            | Expr.FunCall (f, e) when f.ToLower() = "sin" ->
                Eval env e |> sin
            | Expr.FunCall (f, e) when f.ToLower() = "cos" ->
                Eval env e |> cos
            | Expr.FunCall (f, _) ->
                "Unknown function: " + f |> failwith
