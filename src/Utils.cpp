// #include <Rcpp.h>
#include <map>

#include "Utils.h"

extern "C" {
    // from inp into ans (from attrib.c)
    void copyMostAttrib(SEXP inp, SEXP ans);
}

// this is the map, used for storing the hashes
std::map<size_t, bool> seenSigs;
// this is the str version of the std hash function
std::hash<std::string> str_hash;

std::string make_into_string(SEXP s) {
  return CHAR(STRING_ELT(s, 0));
}

// [[Rcpp::export]]
std::string is_interesting(std::string pkg_name, std::string fun_name, int arg_len, SEXP arg_names,
                    SEXP arg_types, SEXP arg_attrs, SEXP arg_classes) {
  // NOTE: ensure that arg_attrs passed as a list of strings, i.e. collate already

  // first, compute hash
  // first.1: make a big string
  std::string aggr_string = pkg_name + ":" + fun_name + "/";

  for (int i = 0; i < arg_len; i++) {
    // aggr_string = CHAR(STRING_ELT((VECTOR_ELT(arg_names, i)), 0));
    aggr_string = aggr_string + make_into_string(VECTOR_ELT(arg_names, i)) + "," + make_into_string(VECTOR_ELT(arg_types, i)) +
                  "," + make_into_string(VECTOR_ELT(arg_attrs, i)) + "," + make_into_string(VECTOR_ELT(arg_classes, i)) + "/";
  }

  // first.2: hash it
  size_t hashedSig = str_hash(aggr_string);

  // second, see if hash collision occurs
  // bool collision = false;
  std::string interesting = "1_";

  if (seenSigs.find(hashedSig) != seenSigs.end()) {
    // in
    // collision = true;
    interesting = "0_";
  } else {
    // out, add it
    seenSigs[hashedSig] = true;
  }

  return interesting + std::to_string(hashedSig);
}

// [[Rcpp::export]]
SEXP get_dd_val(int i, SEXP rho, SEXP default_value, bool force=false) {
    // TODO: check args
    SEXP dots = findVar(R_DotsSymbol, rho);

    if (TYPEOF(dots) == DOTSXP && dots != R_UnboundValue) {
        if (length(dots) >= i) {
            dots = nthcdr(dots, i - 1);
            SEXP val = CAR(dots);

            if (TYPEOF(val) == PROMSXP) {
                if (force) {
                    return Rf_eval(val, rho);
                } else if (PRVALUE(val) == R_UnboundValue) {
                    return default_value;
                } else {
                    return PRVALUE(val);
                }
            } else {
                return val;
            }
        } else {
            Rf_error("Unable to find ..%d - the ... does not contain %d elements", i, i);
        }
    } else {
        Rf_error("Unable to find ..%d - used in an incorrect context, no ... to look in", i);
    }

    return default_value;
}

// [[Rcpp::export]]
SEXP reassign_function(SEXP target_fun, SEXP new_fun) {
  // changed this to also support builtins, hopefully this works
  if (TYPEOF(target_fun) != CLOSXP && TYPEOF(target_fun) != BUILTINSXP) error("target_fun must be a function");
  if (TYPEOF(new_fun) != CLOSXP && TYPEOF(new_fun) != BUILTINSXP) error("new_fun must be a function");

  SET_BODY(target_fun, BODY(new_fun));

  return R_NilValue;
}

// [[Rcpp::export]]
SEXP create_duplicate(SEXP target) {
  if (isNull(target)) error("target must not be null");

  return duplicate(target);
}

// [[Rcpp::export]]
std::string environment_name(SEXP env) {
    if (R_IsPackageEnv(env) == TRUE) {
        // cf. builtin.c:432 do_envirName
        return CHAR(STRING_ELT(R_PackageEnvName(env), 0));
    } else if (R_IsNamespaceEnv(env) == TRUE) {
        // cf. builtin.c:434 do_envirName
        return CHAR(STRING_ELT(R_NamespaceEnvSpec(env), 0));
    } else {
        return "";
    }
}

// [[Rcpp::export]]
std::string environment_name_as_code(SEXP env) {
    if (env == R_EmptyEnv) {
        return "emptyenv()";
    } else if (env == R_GlobalEnv) {
        return ".GlobalEnv";
    } else if (env == R_BaseEnv || env == R_BaseNamespace) {
        return ".BaseNamespaceEnv";
    } else {
        std::string name = environment_name(env);
        if (!name.empty()) {
            return "getNamespace(\"" + name + "\")";
        } else {
            return "";
        }
    }
}
