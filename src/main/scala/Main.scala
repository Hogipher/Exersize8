@main def main: Unit =

  package base

  object ex8 {


    def eval(e: Expr, env: Env): Int = e match {
      case Input => predefinedInput()
      case Num(n) => n
      case Neg(e) => eval(e,env)*(-1)
      case Plus(e1,e2) => eval(e1,env)+eval(e2,env)
      case Minus(e1,e2) => eval(e1,env)-eval(e2,env)
      case Mult(e1,e2) => eval(e1,env)*eval(e2,env)
      case Equals(e1,e2) => {
        if (eval(e1,env) == eval(e2,env)) 1
        else 0
      }
      case Less(e1,e2) => {
        if (eval(e1,env) < eval(e2,env)) 1
        else 0
      }
      case LessEq(e1,e2) => {
        if (eval(e1,env)<=eval(e2,env)) 1
        else 0
      }
      case Or(e1,e2) => {
        if (eval(e1,env) == 1 || eval(e2,env) == 1) 1
        else 0
      }
      case Var(n) => env.lookupVar(n).contents
      case ArrayLookup(n,i) => env.lookupArray(n).locs(eval(i,env)).contents

      // implement remaining cases here (all are Part A)

      case _ =>
        error(s"Don't know how to eval $e")
    }

    def exec(s: Stmt, env: Env): Unit = s match {
      case Output(expr) => predefinedOutput(expr, env)
      case If(cond,thn,els) => els match {
        case Some(els) => {
          if (eval(cond,env)!=0) exec(thn,env)
          else exec(els,env)
        }
        case None => if (eval(cond,env)==1) exec(thn,env)
      }
      case While(cond,b) => {
        while(eval(cond,env)!=0){
          exec(b,env)
        }
      }
      case Block(declsLst,sLst) => {
        def declPart(declsLst:List[Decl],env:Env):Env = {
          if (declsLst.length == 0) env
          else declPart(declsLst.tail,declare(declsLst.head,env))
        }
        def sPart(sLst:List[Stmt],newEnv:Env):Unit = {
          if (sLst.length == 0) return
          else {
            exec(sLst.head,newEnv)
            sPart(sLst.tail,newEnv)
          }
        }
        val newEnv:Env = declPart(declsLst,env)
        sPart(sLst,newEnv)
      }
      case Assign(n,e) => env.lookupVar(n).contents = eval(e,env)
      case ArrayAssign(n,i,e) => env.lookupArray(n).locs(eval(i,env)).contents = eval(e,env)
      case ProcedureCall(n,a) => {
        //env.lookupProcedure(n).params
        if (a.isEmpty) exec(env.lookupProcedure(n).body,env)
        else{
          def paramToVarMapper(args:List[Expr],paramEnv:Env,
            origEnv:Env,params:List[Param]):Env = args match {
            case List() => paramEnv
            case args => params.head match {
              case Param(s,false) => args.head match {
                case Var(n) => {
                  //println((origEnv.lookupVar(n).contents,"val of var"))
                  val varVal:Int = origEnv.lookupVar(n).
                  contents;
                  paramToVarMapper(args.tail,paramEnv.insert
                    (s,new Location(varVal)),origEnv,params.tail);
                }
                case _ => paramToVarMapper(args.tail,paramEnv.insert
                  (s,new Location(eval(args.head,paramEnv))),origEnv,params.tail);
              }
              case Param(s,true) => args.head match {
                case Var(n) => paramToVarMapper(args.tail,paramEnv.
                  insert(s,origEnv.lookupVar(n)),origEnv,params.tail);
                case ArrayLookup(n,i) => paramToVarMapper(args.tail,
                  paramEnv.insert(params.head.name,origEnv.lookupArray(n).
                    locs(eval(i,origEnv))),origEnv,params.tail);
                case _ =>
                  paramToVarMapper(args.tail,paramEnv,origEnv,params.tail);
                   // This is here to get rid of an error saying "match may not be exhaustive"
              }
            }
          }
        val paramEnv:Env = paramToVarMapper(a,Env.empty,env,env.lookupProcedure(n).params)
        exec(env.lookupProcedure(n).body,paramEnv)
        }
      }

      // implement remaining Part A statements here: If/While/Block/Assign/ArrayAssign

      // eventually implement ProcedureCall for Part B

      case _ =>
        error(s"Don't know how to exec $s")
    }

    def declare(d: Decl, env: Env): Env = d match {
      // implement Part A declarations here: VariableDecl/ArrayDecl
      case VariableDecl(n,i) => env.insert(n, new Location(eval(i,env)))
      case ArrayDecl(n,s) => {
        def mapMaker(i:Int,size:Int,map:Map[Int,Location]):Map[Int,Location] = {
          if (i>=size) map
          else mapMaker(i+1,size,map+(i -> new Location(0)))
        }
        val locations:Map[Int,Location] = mapMaker(0,eval(s,env),Map().empty)
        env.insert(n,new MemoryBlock(locations))
      }
      // eventually implement ProcedureDecl for Part B
      case ProcedureDecl(n,p,b) => env.insert(n,new Closure(n,p,b))
      //case ProcedureDecl(n,p,b) => env.insert(n, new Closure(n,p,b))

      case _ =>
        error(s"Don't know how to declare $d")
    }
  }
