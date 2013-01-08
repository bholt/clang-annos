#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/Type.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"
#include "NodeTyper.h"
#include "TyperVisitor.h"
#include "llvm/Support/Debug.h"
#include <iostream>

// A random number that's unlikely to come up in source programs...
#define TAG_ENDORSEMENT 9946037276205

using namespace clang;

namespace {

// The types in the grappa type system.
typedef enum {
  GrappaInvalid = -1,
  GrappaNone = 0,
  GrappaGlobal
} GrappaQualifier;

// The typer: assign types to AST nodes.
class GrappaTyper : public NodeTyper<GrappaQualifier> {
public:
  virtual GrappaQualifier typeForQualString(llvm::StringRef s);
  virtual GrappaQualifier defaultType(clang::Decl *decl);
  virtual GrappaQualifier typeForExpr(clang::Expr *expr);
  virtual uint32_t flattenType(GrappaQualifier type);
  virtual void checkStmt(clang::Stmt *stmt);

  GrappaTyper(TyperConsumer *_controller) :
    NodeTyper<GrappaQualifier>(_controller) {}

private:
  void checkCondition(clang::Expr *cond);
  GrappaQualifier checkAssignment(GrappaQualifier ltype, clang::Expr *expr);
  GrappaQualifier checkBinop(GrappaQualifier ltype, clang::Expr *expr);
};

// Plugin hooks for Clang driver.
class GrappaTypeCheckerAction: public PluginASTAction {
protected:
  ASTConsumer *CreateASTConsumer( CompilerInstance &CI, llvm::StringRef ) {
    // Pass the type-checking visitor back to Clang so we get called to
    // traverse all ASTs.
    return new TyperVisitor<GrappaQualifier, GrappaTyper>( CI );
  }

  bool ParseArgs( const CompilerInstance &CI,
                  const std::vector<std::string>& args ) {
    // No arguments.
    return true;
  }

  void PrintHelp( llvm::raw_ostream& ros ) {
    ros << "TODO\n";
  }

  virtual ~GrappaTypeCheckerAction() {
  }
};

} // end namespace

// Register the plugin.
static FrontendPluginRegistry::Add<GrappaTypeCheckerAction> X(
    "grappa-type-checker", "perform Grappa type checking" );


// Assign qualifiers to declarations based on type annotations.
GrappaQualifier GrappaTyper::typeForQualString(llvm::StringRef s) {
  if (s.equals("global")) {
    return GrappaGlobal;
  } else {
    // NOTE: may need to just ignore them so we can use more than one checker at a time
    std::cerr << "INVALID QUALIFIER: " << s.str() << "\n";
    return GrappaInvalid;
  }
}

// Default type for unannotated declarations.
GrappaQualifier GrappaTyper::defaultType(clang::Decl *decl) {
  return GrappaNone;
}

GrappaQualifier GrappaTyper::checkBinop(GrappaQualifier ltype, clang::Expr *expr) {
  if (ltype != typeOf(expr)) {
    typeError(expr, "Arithmetic between non-global pointer and global pointer.");
    return GrappaGlobal;
  } else {
    return ltype;
  }
}

GrappaQualifier GrappaTyper::checkAssignment(GrappaQualifier ltype, clang::Expr *expr) {
  if (ltype == GrappaGlobal && typeOf(expr) != GrappaGlobal) {
    typeError(expr, "Assignment of non-global pointer to global pointer.");
    return GrappaGlobal;
  } else if (ltype != GrappaGlobal && typeOf(expr) == GrappaGlobal) {
    typeError(expr, "Assignment of global pointer to non-global pointer.");
    return GrappaGlobal;
  } else {
    return ltype;
  }
}

// Give types to expressions.
GrappaQualifier GrappaTyper::typeForExpr(clang::Expr *expr) {
  // Tolerate null.
  if (!expr)
    return GrappaNone;

  switch ( expr->getStmtClass() ) {

  // LITERALS
  case clang::Stmt::CharacterLiteralClass:
  case clang::Stmt::IntegerLiteralClass:
  case clang::Stmt::StringLiteralClass:
  case clang::Stmt::ImaginaryLiteralClass:
  case clang::Stmt::CompoundLiteralExprClass:
  case clang::Stmt::CXXBoolLiteralExprClass:
  case clang::Stmt::CXXNullPtrLiteralExprClass:
  case clang::Stmt::GNUNullExprClass:
  case clang::Stmt::FloatingLiteralClass:
  case clang::Stmt::UnaryTypeTraitExprClass:
  case clang::Stmt::CXXConstructExprClass:
  case clang::Stmt::CXXNewExprClass:
  case clang::Stmt::CXXDeleteExprClass:
  case clang::Stmt::OffsetOfExprClass:
  case clang::Stmt::SizeOfPackExprClass:
    return GrappaNone;

  // VARIABLE REFERENCES
  case clang::Stmt::DeclRefExprClass: {
    clang::DeclRefExpr* tex = cast<clang::DeclRefExpr>(expr);
    DEBUG(llvm::errs() << "variable reference");
    DEBUG(tex->getDecl()->dump());
    DEBUG(llvm::errs() << " -- " << typeOf(tex->getDecl()) << "\n");
    return typeOf(tex->getDecl());
  }
  case clang::Stmt::DependentScopeDeclRefExprClass: {
    DEBUG(llvm::errs() << "UNSOUND: template argument untypable\n");
    return GrappaNone;
  }
  case clang::Stmt::MemberExprClass: {
    clang::MemberExpr* mex = cast<clang::MemberExpr>(expr);
    DEBUG(mex->getMemberDecl()->dump());
    DEBUG(llvm::errs() << ": member is " <<
          typeOf(mex->getMemberDecl()) << "\n");
    return typeOf(mex->getMemberDecl());
  }

  // CASTS
  // In all cases, we just propagate the qualifier of the casted expression.
  // This is especially important for implicit casts, but for explicit casts we
  // might eventually want to allow qualifiers -- but that should probably only
  // be done with endorsements.
  case clang::Stmt::ImplicitCastExprClass:
  case clang::Stmt::CStyleCastExprClass:
  case clang::Stmt::CXXFunctionalCastExprClass:
  case clang::Stmt::CXXConstCastExprClass:
  case clang::Stmt::CXXDynamicCastExprClass:
  case clang::Stmt::CXXReinterpretCastExprClass:
  case clang::Stmt::CXXStaticCastExprClass:
    return typeOf(cast<clang::CastExpr>(expr)->getSubExpr());


  // BINARY OPERATORS
  case clang::Stmt::BinaryOperatorClass: {
    clang::BinaryOperator *bop = llvm::cast<BinaryOperator>(expr);
    DEBUG(llvm::errs() << "binary operator " << bop->getOpcode() << "\n");
    DEBUG(bop->getLHS()->dump());
    DEBUG(bop->getRHS()->dump());
    GrappaQualifier ltype = typeOf(bop->getLHS());
    GrappaQualifier rtype = typeOf(bop->getRHS());

    DEBUG(llvm::errs() << "left " << ltype << " right " << rtype << "\n");

    switch (bop->getOpcode()) {
      case BO_Mul:
      case BO_Div:
      case BO_Rem:
      case BO_Add:
      case BO_Sub:
      case BO_Shl:
      case BO_Shr:
      case BO_LT:
      case BO_GT:
      case BO_LE:
      case BO_GE:
      case BO_EQ:
      case BO_NE:
      case BO_And:
      case BO_Xor:
      case BO_Or:
      case BO_LAnd:
      case BO_LOr:
        DEBUG(llvm::errs() << "arithmetic\n");
        return checkBinop(ltype, bop->getRHS());

      case BO_Assign:
      case BO_MulAssign:
      case BO_DivAssign:
      case BO_RemAssign:
      case BO_AddAssign:
      case BO_SubAssign:
      case BO_ShlAssign:
      case BO_ShrAssign:
      case BO_AndAssign:
      case BO_XorAssign:
      case BO_OrAssign: {
        DEBUG(llvm::errs() << "assignment\n");
        return checkAssignment(ltype, bop->getRHS());
      }
      case BO_Comma: {
        // An ordinary comma expression.
        DEBUG(llvm::errs() << "normal comma " << ltype << " " << rtype << "\n");
        return rtype;
      }
      case BO_PtrMemD:
      case BO_PtrMemI:
        // TODO: currently not supporting pointers to members (I think we do already in runtime, but should check first)
        DEBUG(llvm::errs() << "UNSOUND: unimplemented: pointer-to-member\n");
        return GrappaNone;
    }
  }

  // UNARY OPERATORS
  case clang::Stmt::UnaryOperatorClass: {
    clang::UnaryOperator *uop = llvm::cast<UnaryOperator>(expr);
    GrappaQualifier argt = typeOf(uop->getSubExpr());
    switch (uop->getOpcode()) {
      case UO_PostInc:
      case UO_PostDec:
      case UO_PreInc:
      case UO_PreDec:
      case UO_Plus:
      case UO_Minus:
      case UO_Not:
      case UO_LNot:
      case UO_AddrOf:
      case UO_Deref:
        return GrappaNone;
        
      case UO_Real: // funky complex stuff
      case UO_Imag:
      case UO_Extension:
        return argt;
    }
  }


  // ARRAYS
  case clang::Stmt::ArraySubscriptExprClass: {
    // Array member has same precision as the array.
    ArraySubscriptExpr* texp = cast<ArraySubscriptExpr>( expr );
    return typeOf(texp->getBase());
  }


  // FUNCTION CALLS
  case clang::Stmt::CXXMemberCallExprClass: 
  case clang::Stmt::CXXOperatorCallExprClass: 
  case clang::Stmt::CallExprClass: {
    clang::CallExpr* call = cast<CallExpr>(expr);

    clang::FunctionDecl *callee = call->getDirectCallee();
    if (!callee) {
      DEBUG(llvm::errs() << "UNSOUND: could not find callee for ");
      DEBUG(call->dump());
      DEBUG(llvm::errs() << "\n");
      return GrappaInvalid;
    }

    // Special cases for libc allocation functions (i.e., permissive
    // "annotations" for the standard library).
    llvm::StringRef funcName = callee->getName();
    if (funcName == "free" || funcName == "memcpy") {
      return GrappaNone;
    }

    // Check parameters.
    // XXX variadic, default args: when param/arg list lengths are not equal
    clang::FunctionDecl::param_iterator pi = callee->param_begin();
    clang::CallExpr::arg_iterator ai = call->arg_begin();
    for (; pi != callee->param_end() && ai != call->arg_end(); ++pi, ++ai) {
      GrappaQualifier paramType = typeOf(*pi);
      GrappaQualifier argType = typeOf(*ai);
      if (paramType != argType) {
        typeError(call, "precision flow violation");
      }
    }

    DEBUG(llvm::errs() << "callee: ");
    DEBUG(callee->dump());
    DEBUG(llvm::errs() << " type: " << typeOf(callee) << "\n");

    // Set return type qualifier.
    return typeOf(callee);
  }


  // MISC
  case clang::Stmt::ParenExprClass: {
    clang::ParenExpr* paren = cast<ParenExpr>(expr);
    return typeOf(paren->getSubExpr());
  }

                                           
  default:
    DEBUG(llvm::errs() << "UNSOUND: unhandled expression kind "
                       << expr->getStmtClass() << "\n");
    // change to getStmtClassName() for name lookup (crashes for some kinds)
    return GrappaNone;
  }
}

// Flat representation of types.
uint32_t GrappaTyper::flattenType(GrappaQualifier type) {
  return (unsigned int)type;
}

// Checking types in statements. (No type assignment here.)
void GrappaTyper::checkStmt(clang::Stmt *stmt) {
  switch (stmt->getStmtClass()) {
    // Check function return type.
    case clang::Stmt::ReturnStmtClass:
      {
        clang::Expr *expr = cast<clang::ReturnStmt>(stmt)->getRetValue();
        if (expr && curFunction) {
          if (typeOf(expr) != typeOf(curFunction)) {
            typeError(expr, "Grappa pointer scope mismatch in return type.");
          }
        }
      }
      break;

    // Check variable initialization.
    case clang::Stmt::DeclStmtClass:
      {
        clang::DeclStmt *dstmt = cast<clang::DeclStmt>(stmt);
        for (DeclStmt::const_decl_iterator i = dstmt->decl_begin();
             i != dstmt->decl_end(); ++i) {
          clang::VarDecl *vdecl = dyn_cast<clang::VarDecl>(*i);
          if (vdecl && vdecl->hasInit()) {
            checkAssignment(typeOf(vdecl), vdecl->getInit());
          }
        }
      }
      break;

    default:
      break;
  }
}
