print_string "Abstract Syntax Tree of CoreJava\n";;
print_string "A program can be represented as a dictionary of classes (association list).\n";;

(*--------------------ASSIGN1-------------------*)
type tPrim = Tint | Tfloat | Tbool| Tvoid

(*Variant type = a type that can have more than one form
First part of each | separated part is called the constructor - capital letter.
Used as constant
OR
Used to define a value => of type part, where type always starts with a lowercase letter.
The type can be:
- primitive type
- class name
- bottom (null)
*)
type typ = 
 Tprim of tPrim
 | Tclass of string 
 | Tbot
 
type value = 
 Vnull (*null value*)
 | Int of int 
 | Float of float
 | Bool of bool 
 | Vvoid (*value of type void*)

type opcm  =
 Eq (*==*)
 | NotEq (*!=*)
 | GrThan (*>*)
 | GrEqThan (*>=*)
 | LessThan (*<*)
 | LessEqThan (*<=*)

(*call parameters list having an identifier (name)*)
type param = VarParam of string
| ValParam of value
type callPrmList = param list

type blkExp =
 Bvar of typ*string*exp
 | Bnvar of exp
 | Blk of blkExp
 and 
 (*Recursive variant*)
 exp = 
 Value of value
 | Var of string (*variable name*)
 | Vfld of string * string (*field of an object*)
 | AsgnV of string*exp (*variable assignment*)
 | AsgnF of string*string*exp (*object field assignment*)
 | Seq of exp*exp
 | BlkExp of blkExp
 | If of string*blkExp*blkExp (*conditional*)
 (*arithmetic expressions*)
 | AddInt of exp*exp (* means a + b *)
 | SubInt of exp*exp (* means a - b *)
 | MulInt of exp*exp (* means a * b *)
 | DivideInt of exp*exp (* means a / b *)
 | AndOp of exp*exp (*logical expressions*)
 | OrOp of exp*exp
 | NotOp of exp
 | Relational of exp*opcm*exp (*relational expressions*)
 | NewInst of string* callPrmList
 | MethodCall of string*string*callPrmList
 | While of string*blkExp (*while loop*)
 | Cast of typ*string
 | InstanceOf of string*typ
 
 (*function declaration parameters, having a type and an identifier (name)*)
type fPrm = (typ*string)

type fPrmList = fPrm list

(*Method declaration
A method consists of:
- type of the result
- method name
- method parameters (linked list)
- body (expression)*) 
type mthDecl =(typ*string*fPrmList*blkExp)
 
type mthDeclList = mthDecl list

(*Tuples can contain elements of different types
The field declaration is a pair of:
- type: field type
- string: field name
*)
type fldDecl = (typ * string) 

(*Field declaration- type, field name*)
type fldDeclList = fldDecl list

(*Class declaration
Consists of:
- current class name
- parent class name
- body - field declarations and methods declarations*)
type classDecl = (string*string*fldDeclList*mthDeclList)

(*Program
Consists of a list of class declarations (at least one)*)
type progr = (string*classDecl) list
 
(*if m then {z=z+1} else {z=2}*)
let ifexp1 = (If ("m",
 (Blk (Bnvar (AsgnV ("z", (AddInt (Var "z",(Value (Int
1)))))))),
 (Blk (Bnvar (AsgnV ("z",(Value (Int 2)) ))))
 )) 
 
(*class A extends Object{
int f1;
#
int m1(int a, int b) { (int c)
 c=a+b;this.f1=this.f1+c;c};
} *)
let assign1 = 
AsgnV("c", AddInt(Var "a", Var "b"))
let assign2 =
AsgnF("this", "f1", AddInt(Vfld("this", "f1"), Var "c"))
let body = Blk(
Bvar(Tprim(Tint), "c", 
Seq(Seq(assign1, assign2), Var "c")))
let prm1A = (Tprim(Tint), "a")
let prm2A = (Tprim(Tint), "b")
let mthA = (Tprim(Tint), "m1", [prm1A; prm2A], body)
let mthDeclA = [mthA]

let fldA1 = (Tprim(Tint), "f1")
let fldDeclA = [fldA1]
let classA = ("A", "Object", fldDeclA, mthDeclA)

(*class B extends A{
A f2;
#
A m2(A x, A y) {(A z) { (int n)
 n=x.m1(1,2)-y.m1(2,1);
 {(bool m) m=(x.f1-y.f1)>n;
 if m then {z=new A(m)} else {z=new A(n)}
 }
 };this.f2=z;z
} *)
let callParams1 = [ValParam(Int 1); ValParam(Int 2)]
let callParams2 = [ValParam(Int 2); ValParam(Int 1)]
let callX = MethodCall("x", "m1", callParams1)
let callY = MethodCall("y", "m1", callParams2)
let expr1B = AsgnV("n", SubInt( callX, callY))

let newA1 = NewInst("A", [VarParam("m")])
let newA2 = NewInst("A", [VarParam("n")])
let ifExprB = 
(If ("m",
 (Blk (Bnvar (AsgnV ("z", newA1 )))),
 (Blk (Bnvar (AsgnV ("z", newA2 ))))
 )) 
let expr2B = Blk(Bvar(Tprim(Tbool), "m", ifExprB))

let expr3B = AsgnF("this", "f2", Var "z")

let expr4B = Blk(Bvar(Tprim(Tint), "n", Seq(expr1B, BlkExp(expr2B))))
let body = Blk(Bvar(Tclass("A"), "z", 
		   Seq(Seq(BlkExp(expr4B), expr3B), Var "z")))

let prm1B = (Tclass("A"), "x")
let prm2B = (Tclass("A"), "y")
let methB = (Tclass("A"), "m2", [prm1B; prm1B], body)
let mthDeclB = [methB]

let fldB1 = (Tclass("A"), "f2")
let fldDeclB = [fldB1]
let classB = ("B", "A", fldDeclB, mthDeclB)

(*Class Main extends Object{ #
Void main(){ (B o1) o1=new B(0,null);
 { (A o2) o2=new A(2);
 { (A o3) o3=new A(3);
 o2 =o1.m2(o2,o3)
 }
 }
 }
}*)
let e1 = AsgnV("o3", NewInst("A", [ValParam(Int 3)]))
let e2 = AsgnV("o2", MethodCall("o1", "m2", [VarParam("o2"); VarParam("o3")]))
let inBlk = Blk(Bvar(Tclass("A"), "o3", Seq(e1, e2)))
let ins2 = AsgnV("o2", NewInst("A", [ValParam(Int 2)]))
let outBlk = Blk(Bvar(Tclass("A"), "o2", Seq(ins2, BlkExp(inBlk))))

let ins1 = AsgnV("o1", NewInst("B", [ValParam(Int 0); ValParam(Vnull)]))
let outOutBlk = Blk(Bvar(Tclass("B"), "o1", Seq(ins1, BlkExp(outBlk))))
let bodyMain = Blk(Bnvar(BlkExp(outOutBlk)))

let mthMain = (Tprim(Tvoid), "main", [], bodyMain)
let mthDeclMain = [mthMain]
let classMain = ("Main", "Object", [], mthDeclMain)

let program = [("P",classA); ("P",classB); ("P",classMain)]

(*ASSIGNMENT 2 - check subtype
Subtype (prog: progr) (t1:typ) (t2:typ) = ...
 It returns a bool
- it has a different body according to the patterns of t1 and t2

Subtype (prog: progr) (t1:typ) (t2:typ)
*)

(*
(*function call*)
(*should return false*)
Subtype program Tclass("A") Tclass("B") 
(*should return true*)
Subtype program Tclass("B") Tclass("A") 
(*should return true*)
Subtype program Tclass("B") Tclass("B") 
(*should return true*)
Subtype program Tprim(TInt) TPrim(TInt)
(*should return true*)
Subtype program Tprim(Tfloat) TPrim(Tfloat)
(*should return true*)
Subtype program Tprim(Tbool) TPrim(Tbool)
(*should return true*)
Subtype program Tprim(Tvoid) TPrim(Tvoid)

(*ASSIGNMENT 2 
Fieldlist (prog: progr) (classname: string) =
It returns a list of pairs field name and type: (string*typ) list
*)
Fieldlist (prog: progr) (classname: string)
//TODO

(*function call*)
(*should return false*)
Fieldlist program "A"

(*ASSIGNMENT 2 
TypeCheckExp (prog: progr) (environment: (string*typ) list) (expCrt: exp)=
-It returns typ or throws exception when a condition does not hold
- its body depends on the pattern of the current expression expCrt
*)
TypeCheckExp (prog: progr) (environment: (string*typ) list) (expCrt: exp)
//TODO

(*function call*)
let expressionT = AddInt ((Value (Int 2)),(Value (Int 1)))
TypeCheckExp program [("P", Tprim(int))] expressionT
*)
