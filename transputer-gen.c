/*
 *  Tansputer code generator for TCC
 *
 */

/* number of available registers */

#ifdef TARGET_DEFS_ONLY

#define NB_REGS 14

#define RC_ARG 0x0003      /* Argument pointer */
#define RC_IPTR 0x0030     /* Frame pointer */

/* a register can belong to several classes. The classes must be
   sorted from more general to more precise (see gv2() code which does
   assumptions on it). */
   
#define RC_IST 0x0000       /*  any int stack entry */
#define RC_IST0 0x0001      /*  top of int stack */
#define RC_IST1 0x0002      /*  top int stack - 1 */
#define RC_IST2 0x0004      /*  top int stack - 2 */

#define RC_FST 0x0010       /*  any float stack entry */
#define RC_FST0 0x0020      /*  top of float stack */
#define RC_FST1 0x0040      /*  top float stack - 1 */
#define RC_FST2 0x0080      /*  top float stack - 2 */

#define RC_WREG 0x0100      /*  workspace register */
#define RC_INT 0x0007       /*  generic integer register */
#define RC_FLOAT 0x0070     /*  generic float register */

#define RC_WORKREGS 0x0700  /*  Workspace registers (through Regions) */
#define RC_BASEREGS 0x0707  /*  Base registers */
#define RC_STACKREGS 0x0077 /*  Stack registers */
#define RC_ALLREGS 0x0777   /*  All registers in region */

#define REG_IRET RC_IST0    /*  single word int return register */
#define REG_IRE2 RC_IST1    /*  second word return register (for long long) */
#define REG_FRET RC_FST0    /* 	64 bit float return register */

#define BETWEEN(v1, l1, v2, l2)\
if !((v1 + l1 >= v2 && v1 <= v2) || (v2 + l2 >= v1 && v2 <= v1))

/* defined if function parameters must be evaluated in reverse order */
/* #define INVERT_FUNC_PARAMS */

/* defined if structures are passed as pointers. Otherwise structures
   are directly pushed on stack. */
/* #define FUNC_STRUCT_PARAM_AS_PTR */

/* pointer size, in bytes */
#define PTR_SIZE 4

/* long double size and alignment, in bytes */
#define LDOUBLE_SIZE  12
#define LDOUBLE_ALIGN 4

/* maximum alignment (for aligned attribute support) */
#define MAX_ALIGN     8

#undef CONFIG_TCC_BCHECK

/******************************************************/
#else /* ! TARGET_DEFS_ONLY */
/******************************************************/
#define USING_GLOBALS
#include "tcc.h"

// Fix for implicit declaration of 'g' function
#include "tcc.h"
void oad(int ind) {
    g((0x02 << 12) | \
      (0x02 << 6) | \
      (ind & 0x3f));
}

#define DEFAULT_ALIGN 1

#define BITS_BIG_ENDIAN 0

#define PTR_SIZE 4

#define DBL_EPSILON 2.2204460492503131e-16
#define FLT_EPSILON 1.19209290e-07 F

#undef BOOL
#define BOOL int

#ifndef FALSE
# define FALSE 0
# define TRUE 1
#endif

#define ALIGNEMENT DEFAULT_ALIGN

/* pretty names for the registers */
enum {
  REG_IST0 = 0,
    REG_IST1,
    REG_IST2,
    REG_FST0,
    REG_FST1,
    REG_FST2,
    REG_WREG,
    REG_ARG,
    REG_IPTR,
    REG_INT,
    REG_FLOAT,
    REG_WORKREGS,
    REG_BASEREGS,
    REG_STACKREGS,
    REG_ALLREGS
};

ST_DATA const int reg_classes[NB_REGS] = {
  /* IST0 */
  RC_IST | RC_IST0,
  /* IST1 */
  RC_IST | RC_IST1,
  /* IST2 */
  RC_IST | RC_IST2,
  /* FST0 */
  RC_FST | RC_FST0,
  /* FST1 */
  RC_FST | RC_FST1,
  /* FST2 */
  RC_FST | RC_FST2,
  /* WREG */
  RC_WREG,
  /* ARG  */
  RC_ARG,
  /* FRP  */
  RC_IPTR,
  /* GEN  */
  RC_INT,
  /* GENF */
  RC_FLOAT,
  /* WRKs */
  RC_WORKREGS,
  /* BASE */
  RC_BASEREGS,
  /* STACK*/
  RC_STACKREGS,
  /* ALL  */
  RC_ALLREGS
};

static char * reg_names[] = {
  "Areg",
  "Breg",
  "Creg",
  "FAreg",
  "FBreg",
  "FCreg",
  "Wreg",
  "Arg",
  "Iprt",
  "Gen",
  "Flt",
  "Wrgs",
  "Bregs",
  "Sregs",
  "Allregs"
};

static unsigned long func_sub_sp_offset;
static int func_ret_sub;

/******************************************************/
/* opcode definitions */
/******************************************************/

enum TPOPCodes {
  #define OP(name, str, n) TP_OP_##name = n,
  #include "transputer-opcodes.h"
  #undef OP
};

char *tp_opcodes_str[] = {
  #define OP(name, str, n) [n] = str,
  #include "transputer-opcodes.h"
  #undef OP
};

int get_prefix(int v) {
  if (v < 16)
    return 0x00;
  else if (v < 0)
    return -v >> 4;
  return v >> 4;
}

/******************************************************/

static uint32_t BigtoLittle32(uint32_t value) {
  uint32_t result = 0;

  result |= (value & 0x000000FF) << 24;
  result |= (value & 0x0000FF00) << 8;
  result |= (value & 0x00FF0000) >> 8;
  result |= (value & 0xFF000000) >> 24;

  return result;
}

#define NoCallArgsPassedOnStack 10
int NoOfCurFuncArgs;
int TranslateStackToReg[NoCallArgsPassedOnStack];
int ParamLocOnStack[NoCallArgsPassedOnStack];

/******************************************************/

// The memory code consists of pfix, opr and the opcode
// if it's a secondary function we need opr as operation code

// E.g. 
// direct function: ldc 40 = pfix 4; ldc 0 = 0x2440
// Indirect function: ret = pfix 2; opr 0 = 0x22F0
// shl = 0x41 = pfix 4 opr 1 = 0x24F1

// Question: How does TCC interact through paging and mapping 
// with memory access (org, alignment, ...), mapped registers, local var, etc. ?

// Important functions: gv, gv2, tccgen_compile
// Important files: tcc_gen

// Are those actually stored in TCC structures SValue, CValue, vtop, ...?
// Direct assembly code would be much easier to handle.

// c67, tccgen are good references how to implement the code generator
// tccgen is the meta-level of the tcc code generator

/* 
 *	All transputers address bytes
 * 	from org 0x800000 (roles over after all 32 bit set) 
 * 	to 0x7FFFFF where 0x000000 is the middle
 */

/*
 * 	In protected mode 32 bit with align = 4 (through goprot), 
 *	the memory regions are 
 *	logical mapped through the region descriptors. The region 
 * 	registers are holding beside bit flags for page 
 *	marking (privilege level, cachable, dirty, readable, etc.) 
 * 	logical memory locations and flag registers for the integer and FP 
 *	stack registers in the P-state data structure (PDS). It's similar
 *	to the x86 IA32 legacy and protected mode where context switching 
 *	is handle by the hardware and we have to initilaize all necessary
 *	data structures into memory and load the logical address into 
 *	the descriptor registers before we switch to protected mode. All the 
 *	mapping is already done (in long mode we must map all needed regions
 *	before we enter long mode with a long jump) through the region registers.
 *	The memory management breaks down to all the information held in
 *	region0-region3 almost like cr3, PAT and MTRRs holding paging informations in x86. 

 *	Caching gets divided into memory cache and workspace cache, which is 
 *	mostly write-through. The mechanism of caching is similar to the of x86
 *	with some special informations about the workspace cache.
 * 	We only need to care about the allocated memory space for the compiler
 *	process, since from ring-levels we are at the
 */

/* output a symbol and patch all calls to it */
ST_FUNC void gsym_addr(int t_, int a_) {
  uint32_t t = t_;
  uint32_t a = a_;
  printf("gsym_addr(t=%d, a=%d)\n", t, a);

  while (t) {
    unsigned char * ptr = cur_text_section -> data + t;
    uint32_t next = read32le(ptr);
    uint32_t r = a - t, imm;

    if (!((a - t + 0x800000 < 0x100000000ULL) || (a - t < 0x7FFFFF)))
      tcc_error("branch out of range");
    write32le(ptr, a - t == 4 ? 0x63F0 : ((0x02 << 12) | \
      (get_prefix(imm) << 8) | 0xF0 | (imm & 0x0F))); // nop || j imm
    t = next;
  }
}

static int oad(int c, int s) {
  int t;
  if (nocode_wanted)
    return s;
  t = ind;
  g((0x02 << 12) | \
    (get_prefix(s)) << 8 | \
    (c | (s & 0x0F)));

  return t;
}

#define gjmp2(instr, lbl) oad(instr, lbl)

int get_primitive_type_size(int bt) {
  int size = 0;
  switch (bt) {
    case VT_FLOAT:
      size = 4;
      break;
    case VT_DOUBLE:
      size = 8;
      break;
    case VT_LDOUBLE:
      tcc_error("long double not supported");
      break;
    case VT_SHORT:
      size = 2;
      break;
    case VT_BYTE:
    case VT_BOOL:
      size = 1;
      break;
    default:
      size = 4;
      break;
  }
  return size;
}

void Transputer_LoadValueToReg(int r, SValue *sv, int bt, int size) {
    int fr = sv->r;
    int fc = sv->c.i;
    int v = fr & VT_VALMASK;
    BOOL Unsigned = FALSE;

    if (fr & VT_LVAL) {
        if (v == VT_LLOCAL) {
            SValue v1;
            v1.type.t = VT_INT;
            v1.r = VT_LOCAL | VT_LVAL;
            v1.c.i = fc;
            Transputer_LoadValueToReg(fr, &v1, bt, size);
            fr = r;
        } else {
            // TODO Implement Transputer instructions for loading values of different sizes
        }
    } else {
        if (v == VT_CONST) {
            if (sv->r & VT_SYM) {
                greloc(cur_text_section, sv->sym, ind, R_TRANSPUTER_IPREL32);
            }

            Transputer_LDC(fc);
        } else if (v == VT_LOCAL) {
            Transputer_LDL(v);
        } else if (v == VT_CMP) {
            Transputer_LDL(v);
            Transputer_REV();
        } else if (v == VT_JMP || v == VT_JMPI) {
            int t = v & 1;
            Transputer_LDC(t);
            gjmp_addr(ind + 4);
            gsym(fc);
            Transputer_LDC(t ^ 1);
        } else {
            Transputer_LDL(v);
        }
    }
}

void Transputer_MoveValueToReg(int r, SValue *sv, int bt, int size) {
    int fr = sv->r;
    int v = fr & VT_VALMASK;

    if (fr & VT_LVAL) {
        Transputer_LoadValueToReg(r, sv, bt, size);
    } else {
        if (v == VT_CONST) {
            if (sv->r & VT_SYM) {
                greloc(cur_text_section, sv->sym, ind, R_TRANSPUTER_IPREL32);
            }

            Transputer_LDC(sv->c.i);
            Transputer_STL(r);
        } else if (v == VT_LOCAL) {
            Transputer_LDL(v);
            Transputer_STL(r);
        } else if (v == VT_CMP) {
            Transputer_LDL(v);
            Transputer_REV();
            Transputer_STL(r);
        } else if (v == VT_JMP || v == VT_JMPI) {
            int t = v & 1;
            Transputer_LDC(t);
            gjmp_addr(ind + 4);
            gsym(sv->c.i);
            Transputer_LDC(t ^ 1);
            Transputer_STL(r);
        } else {
            Transputer_LDL(v);
            Transputer_STL(r);
        }
    }
}

/* load 'r' from value 'sv' */
ST_FUNC void load(int r, SValue * sv) {
  int fr, size = 0;
  CType type = sv->type;
  int bt = type.t & VT_BTYPE;
  fr = sv->r;

  if (fr & VT_LVAL) {
    if ((bt & VT_BTYPE) == VT_STRUCT) {
      int align;
      size = type_size(&type, &align);
      switch (size) {
        case 1:
          bt = VT_BYTE;
          break;
        case 2:
          bt = VT_SHORT;
          break;
        case 4:
          bt = VT_INT;
          break;
        case 8:
          bt = VT_LLONG;
          break;
        default:
          tcc_error("invalid aggregate type for register load");
          break;
      }
    } else {
      size = get_primitive_type_size(bt);
    }
    Transputer_LoadValueToReg(r, sv, bt, size);
  } else {
    Transputer_MoveValueToReg(r, sv, bt, size);
  }
}

void Transputer_StoreValueFromReg(int r, SValue *v, int bt, int size, int ind) {
    int fr = v->r & VT_VALMASK;
    int fc = v->c.i;

    if ((v->r & VT_VALMASK) == VT_CONST) {
        if (v->r & VT_SYM) {
            greloc(cur_text_section, v->sym, ind, R_TRANSPUTER_IPREL32);
        }

        Transputer_LDC(fc);
    } else if ((v->r & VT_VALMASK) == VT_LOCAL) {
        if (fc > 0) {
            int stack_pos = 8;
            int t;

            for (t = 0; t < NoCallArgsPassedOnStack; t++) {
                if (fc == stack_pos) {
                    break;
                }
                stack_pos += TranslateStackToReg[t];
            }

            fc = ParamLocOnStack[t] - 8;
        }

        Transputer_LDL(fc);
    } else {
        Transputer_LDNL(fr);
    }

    if (size == 1) {
        Transputer_LDL(r);
        Transputer_SB();
    } else if (size == 2) {
        Transputer_LDL(r);
        Transputer_SS();
    } else if (size == 4 && bt == VT_INT) {
        Transputer_STL(r);
    } else if (size == 4 && bt == VT_FLOAT) {
        Transputer_LDLP(r);
        Transputer_FPSTNLSN();
    } else {
        Transputer_LDLP(r);
        Transputer_FPSTNLDB();
    }
}

/* store register 'r' in lvalue 'v' */
ST_FUNC void store(int r, SValue *v) {
    int bt, ft, size = 0;

    printf("store(r=%d, v=%p)\n", r, v);

    ft = v->type.t;
    bt = ft & VT_BTYPE;

    switch (bt) {
        case VT_FLOAT:
            size = 4;
            break;
        case VT_DOUBLE:
            size = 8;
            break;
        case VT_LDOUBLE:
            tcc_error("long double not supported");
            break;
        case VT_SHORT:
            size = 2;
            break;
        case VT_BYTE:
        case VT_BOOL:
            size = 1;
            break;
        default:
            size = 4;
            break;
    }

    Transputer_StoreValueFromReg(r, v, bt, size);
}

/* 'is_jmp' is '1' if it is a jump */
static void gcall_or_jmp(int is_jmp) {
  printf("gcall_or_jmp(is_jmp=%d)\n", is_jmp);

  int r;

  if ((vtop -> r & (VT_VALMASK | VT_LVAL)) == VT_CONST && (vtop -> r & VT_SYM)) {
    /* constant and relocation case */
    greloc(cur_text_section, vtop -> sym, ind + 1, R_TRANSPUTER_IPREL32);
    oad(0x09 - (is_jmp * 9), vtop -> c.i - 4); /* call/jmp im */
  } else {
    /* otherwise, indirect call */
    r = gv(RC_INT);
    o(r);
    Transputer_GCALL();
  }
}

int Transputer_ProcessFuncArg(int args_size, CType *type) {
    int size, align, r;

    align = DEFAULT_ALIGN;
    if ((type->t & VT_BTYPE) == VT_STRUCT) {
        size = type_size(type, &align);
        size = (size + 3) & ~3; // align to stack align size
        r = get_reg(RC_INT);
        args_size += size;

        // structure store code
        vset(type, r | VT_LVAL, 0);
        vswap();
        vstore();

    } else if (is_float(type->t & VT_BYTE)) {
        gv(RC_FST); // only one float register
        if ((type->t & VT_BTYPE) == VT_FLOAT)
            size = 4;
        else if ((type->t & VT_BTYPE) == VT_DOUBLE)
            size = 8;

        args_size += size;

        // float store code
        Transputer_LDLP(RC_ARG);
        Transputer_ADC(-args_size);
        if (size == 8) {
            Transputer_FPSTNLDB();
        } else {
            Transputer_FPSTNLSN();
        }

    } else {
        r = gv(RC_INT);
        size = 4; // assuming simple types are always of size 4
        args_size += size;

        // simple type store code
        Transputer_LDL(r);
        Transputer_STL(RC_ARG - args_size);

        if ((type->t & VT_BTYPE) == VT_LLONG) {
            Transputer_LDL(vtop->r2);
            Transputer_STL(RC_ARG - args_size + 4);
        }
    }

    return args_size;
}

/* generate function call with address in (vtop->t, vtop->c) and free function
   context. Stack entry is popped */
ST_FUNC void gfunc_call(int nb_args) {
    printf("gfunc_call(nb_args=%d)\n", nb_args);

    int i, args_size;
    CType *arg_type;

    args_size = 0;

    for (i = 0; i < nb_args; i++) {
        arg_type = &vtop->type;
        args_size = Transputer_ProcessFuncArg(args_size, arg_type);
        vtop--;
    }

    save_regs(0); // save used temporary registers
    Sym *func_sym = vtop->type.ref;
    int func_call = func_sym->f.func_call;

    gcall_or_jmp(0); // generate the function call
    vtop--;

    // return value handling
    if ((func_sym->type.t & VT_BTYPE) != VT_VOID) {
        if (is_float(func_sym->type.t)) {
            if ((func_sym->type.t & VT_BTYPE) == VT_FLOAT) {
                vset(&func_sym->type, RC_FRET, 0);
            } else {
                vset(&func_sym->type, RC_FRET | VT_LVAL, 0);
            }
        } else {
            vset(&func_sym->type, RC_IRET, 0);
            if ((func_sym->type.t & VT_BTYPE) == VT_LLONG) {
                vtop->r2 = RC_IRET2;
            }
        }
    } else {
        vset(&func_sym->type, VT_CONST | VT_VOID, 0);
    }
}

/* Generate function prolog for the given function symbol */
ST_FUNC void gfunc_prolog(Sym *func_sym) {
    printf("//---------------------------------//\n");
    printf("gfunc_prolog(func_type=%p)\n", func_sym);

    CType *func_type = &func_sym->type;
    int address, alignment, size, func_call, index, offset;
    Sym *sym;
    CType *type;

    sym = func_type->ref;
    func_call = sym->f.func_call;
    address = 8;

    // If the function returns a structure, add an implicit pointer parameter
    func_vt = sym->type;
    func_var = (sym->f.func_type == FUNC_ELLIPSIS);

    if ((func_vt.t & VT_BTYPE) == VT_STRUCT) {
        func_vc = address;
        address += 4;
    }

    // Initialize the number of current function arguments
    NoOfCurFuncArgs = 0;

    // Define function parameters
    while ((sym = sym->next) != NULL) {
        type = &sym->type;
        sym_push(sym->v & ~SYM_FIELD, type, VT_LOCAL | VT_LVAL, address);
        size = type_size(type, &alignment);
        size = (size + 3) & ~3;

        // Keep track of the size of arguments
        TranslateStackToReg[NoOfCurFuncArgs] = size;
        NoOfCurFuncArgs++;

        #ifdef FUNC_STRUCT_PARAM_AS_PTR
        // Structs are passed as a pointer
        if ((type->t & VT_BTYPE) == VT_STRUCT) {
            size = 4;
        }
        #endif
        address += size;
    }
    func_ret_sub = 0;

    // Pascal type call
    if (func_call == FUNC_STDCALL)
        func_ret_sub = address - 8;

    // Place all the args passed in registers onto the stack
    loc = 0;

    offset = (address >> 3);

    // Save Integer Stack Register and Iptr
    Transputer_LDL(REG_ARG);
    Transputer_LDNL(1);
    Transputer_LDL(REG_STK);
    Transputer_GAJW();
    Transputer_DUP();
    Transputer_LDNL(REG_ARG);
    Transputer_STL(offset - 3);
    Transputer_DUP();
    Transputer_LDNL(REG_WPTR);
    Transputer_STL(offset - 2);
    Transputer_STL(offset - 1);
    Transputer_AJW(-2);
    Transputer_STL(1);
    Transputer_LDC(ind);
    Transputer_LDC(2);
    Transputer_LDPI();
    Transputer_STL(0);
    Transputer_GCALL();
}

/* Generate function epilog */
ST_FUNC void gfunc_epilog(void) {
    printf("gfun_epilog()\n");
    printf("ret\n");
    printf("//---------------------------------//\n");

    int offset = func_sub_sp_offset;

    // Load a non-local pointer into Areg
    Transputer_LDNL(-loc & -4);
    // Load the size of the stack frame
    Transputer_LDL(offset - 4);
    // Load the number of bytes for the arguments
    Transputer_LDL(offset - 4);
    // Store the Wptr register (sp) into the non-local pointer
    Transputer_STNL(REG_WPTR);
    // Load the number of bytes for the local variables
    Transputer_LDL(offset - 5);
    // Load the number of bytes for the arguments
    Transputer_LDL(offset - 4);
    // Store the arguments into the Wptr register (sp)
    Transputer_STNL(REG_ARG);
    // Add the number of bytes for the arguments to the Wptr register
    Transputer_AJW(offset - 2);
    // Jump to the saved return address
    Transputer_GAJW();
    // Store the saved return address in the stack
    Transputer_STL(REG_STK);
}

/* Fill the code section with NOP instructions */
ST_FUNC void gen_fill_nops(int bytes) {
    if (bytes & 3) {
        tcc_error("alignment of code section not multiple of 4");
    }
    while (bytes > 0) {
        Transputer_NOP(4);
        bytes -= 4;
    }
}

/**
 * Transputer_LD_BLOCK_OP - Load a block of elements from an array and store the result in a register.
 *
 * @param x: The array source.
 * @param b: The destination register where the result will be stored.
 * @param i: The index of the element in the array to be loaded.
 */
void Transputer_LD_BLOCK_OP(SValue * x, int b, int i) {

  Sym* s = x->type.ref;
  int ts = type_size(&s->type, 1);

  if (ts < 0 && s->c < 0) ts = -ts;

  // Check if the source type is a long integer
  if ((x->type.t & VT_BTYPE) == VT_LONG) {
    printf("Loading long array content is not supported!\n");
    return;
  }

  // Check if the source type is a pointer
  if ((x->type.t & VT_BTYPE) == VT_PTR) {
    Transputer_LDL(x);
    Transputer_LDNL(4 * i);
    Transputer_ADD();
    Transputer_STL(b);
  }
  // Check if the source type is an integer
  else if ((x->type.t & VT_BTYPE) == VT_INT) {
    Transputer_LDL(x);
    Transputer_LDNL(ts * i);
    Transputer_STL(b);
  }
  // Check if the source type is a float
  else if ((x->type.t & VT_BTYPE) == VT_FLOAT) {
    Transputer_LDL(x);
    Transputer_LDNL(ts * i);
    Transputer_FPSTNLSN(b);
  }
  // Check if the source type is a double
  else if ((x->type.t & VT_BTYPE) == VT_DOUBLE) {
    Transputer_LDL(x);
    Transputer_LDNL(ts * i);
    Transputer_FPSTNLDB(b);
  } else {
    // handle other types
  }
}

/**
 * Transputer_LDAR - Load an array element and store the result in a register.
 *
 * @param x: The array source.
 * @param b: The destination register where the result will be stored.
 * @param i: The index of the element in the array to be loaded.
 */
void Transputer_LDAR(SValue * x, int b, int i) {

    int ts;
    int a = 1;

    Sym *s;
    s = x->type.ref;

    // Get the type size of the array element
    ts = type_size(&s->type, &a);

    if (ts < 0 && s->c < 0)
        ts = -ts;

    // Check if the source type is a long integer
    if ((x->type.t & VT_BTYPE) == VT_LONG) {
        printf("Loading long array content is not supported!");
    }

    // Check if the source type is a pointer
    if ((x->type.t & VT_BTYPE) == VT_PTR) {
        // Load array index
        Transputer_LDC(i);
        Transputer_LDL(x);

        // Calculate the offset based on the element size
        if (ts == 1)
            Transputer_BSUB();
        else if (ts == 4)
            Transputer_WSUB();
        else if (ts == 8)
            Transputer_WSUBDB();
        else
            Transputer_SSUB();

        // Store the result in b
        Transputer_STL(b);
    }
    // Check if the source type is an integer
    else if ((x->type.t & VT_BTYPE) == VT_INT) {
        // Load the array element value into the accumulator
        Transputer_LDL(x);
        Transputer_LDNL(ts * i);
        // Store the value in b
        Transputer_STL(b);
    } else {
        // Load the element size
        Transputer_LDC(ts);
        // Load the array pointer
        Transputer_LDLP(x);

        // Handle floating-point types
        if ((x->type.t & VT_BTYPE) == VT_DOUBLE) {
            Transputer_FPLDNLDB();
            Transputer_FPSTNLDB(b);
        } else {
            Transputer_FPLDNLSN();
            Transputer_FPSTNLSN(b);
        }
    }
}

/**
 * Transputer_GCALL - Generate a global call instruction.
 *
 * @param n: The target function's address.
 */
void Transputer_GCALL(int n) {
  gen_le16(0x20 << 12 | \
    (get_prefix(n) << 8) | \
    (0xF0 | (0x0F & n)));
}

/**
 * Transputer_LDC_OP - Load a constant value into a specified address.
 *
 * @param a: The address where the constant value will be stored.
 * @param c: The constant value to be loaded.
 */
void Transputer_LDC_OP(int a, int c) {
  Transputer_LDC(c);
  Transputer_LDL(a);
  Transputer_STNL(0);
}

/**
 * Transputer_LDL_OP - Load a value from one address and store it into another address.
 *
 * @param a: The source address from which the value will be loaded.
 * @param b: The destination address where the value will be stored.
 */
void Transputer_LDL_OP(int a, int b) {
  Transputer_LDL(a);
  Transputer_LDNL(0);
  Transputer_LDL(b);
  Transputer_STNL(0);
}

/**
 * Transputer_LADD_OP - Perform long addition of two double-word (64-bit) numbers.
 *
 * @param a: The first operand, a double-word (64-bit) number stored in two registers.
 * @param b: The second operand, a double-word (64-bit) number stored in two registers.
 * @param z: The result of the addition, a double-word (64-bit) number stored in two registers.
 */
void Transputer_LADD_OP(SValue * a, SValue * b, SValue * z) {
  Transputer_LDC(0);

  // Add the low part of the operands and store the result
  Transputer_LDL(a -> r);
  Transputer_LDL(b -> r);
  Transputer_LSUM();
  Transputer_STL(z -> r);

  // Add the high part of the operands and store the result
  Transputer_LDL(a -> r2);
  Transputer_LDL(b -> r2);
  Transputer_LADD();
  Transputer_STL(z -> r2);
}

/**
 * Transputer_LSUB_OP - Perform long subtraction of two double-word (64-bit) numbers.
 *
 * @param a: The first operand (minuend), a double-word (64-bit) number stored in two registers.
 * @param b: The second operand (subtrahend), a double-word (64-bit) number stored in two registers.
 * @param z: The result of the subtraction, a double-word (64-bit) number stored in two registers.
 */
void Transputer_LSUB_OP(SValue * a, SValue * b, SValue * z) {
  Transputer_LDC(0);

  // Subtract the low part of the operands and store the result
  Transputer_LDL(a -> r);
  Transputer_LDL(b -> r);
  Transputer_LDIFF();
  Transputer_STL(z -> r);

  // Subtract the high part of the operands and store the result
  Transputer_LDL(a -> r2);
  Transputer_LDL(b -> r2);
  Transputer_LSUB();
  Transputer_STL(z -> r2);
}

/**
 * Transputer_LMUL_OP - Perform long multiplication of two double-word (64-bit) numbers.
 *
 * @param a: The first operand, a double-word (64-bit) number stored in two registers.
 * @param b: The second operand, a double-word (64-bit) number stored in two registers.
 * @param z: The result of the multiplication, a double-word (64-bit) number stored in two registers.
 */
void Transputer_LMUL_OP(SValue * a, SValue * b, SValue * z) {
  int ra, rb, rz, ra2, rb2, rz2;
  ra = a -> r;
  rb = b -> r;
  rz = z -> r;

  ra2 = a -> r2;
  rb2 = b -> r2;
  rz2 = z -> r2;

  Transputer_LDC(0);

  // Compute low part of the result: ra * rb
  Transputer_LDL(ra);
  Transputer_LDL(rb);
  Transputer_LMUL();
  Transputer_STL(rz);

  // Compute and add the cross-product: ra * rb2
  Transputer_LDL(ra);
  Transputer_LDL(rb2);
  Transputer_LMUL();
  Transputer_REV();
  Transputer_STL(rz);

  // Compute and add the cross-product: ra2 * rb
  Transputer_LDL(ra2);
  Transputer_LDL(rb);
  Transputer_LMUL();
  Transputer_STL(rz2);

  // Compute and add the high part of the result: ra2 * rb2
  Transputer_LDL(ra2);
  Transputer_LDL(rb2);
  Transputer_LMUL();
  Transputer_STL(rz2);

  // Add the computed cross-products to the result
  Transputer_LDC(0);
  Transputer_REV();
  Transputer_LDL(rz);
  Transputer_LSUM();
  Transputer_STL(rz);

  Transputer_LDL(rz2);
  Transputer_LSUM();
  Transputer_STL(rz2);
}

/**
 * Transputer_LDIV_OP - Divide two double-word numbers (64-bit) and store the result.
 *
 * @param a: Pointer to SValue struct holding the dividend (two 32-bit registers).
 * @param b: Pointer to SValue struct holding the divisor (two 32-bit registers).
 * @param z: Pointer to SValue struct to store the result (two 32-bit registers).
 */
void Transputer_LDIV_OP(SValue *a, SValue *b, SValue *z) {
  // Initialize the accumulator with 0
  Transputer_LDC(0);

  // Calculate the denominator (b_hi + b_lo) and store it temporarily
  Transputer_LDL(b->r);
  Transputer_LDL(b->r2);
  Transputer_LSUM();

  // Reverse the operands on the stack
  Transputer_REV();

  // Store the calculated denominator in a temporary register
  Transputer_STL(REG_TMP1);

  // Divide a_hi by the denominator and store the result in z->r
  Transputer_LDL(a->r);
  Transputer_LDL(REG_TMP1);
  Transputer_LDIV();
  Transputer_STL(z->r);

  // Calculate the denominator (b_hi + b_lo) again
  Transputer_LDL(b->r);
  Transputer_LDL(b->r2);
  Transputer_LSUM();

  // Reverse the operands on the stack
  Transputer_REV();

  // Store the calculated denominator in a temporary register
  Transputer_STL(REG_TMP1);

  // Divide a_lo by the denominator and store the result in z->r2
  Transputer_LDL(a->r2);
  Transputer_LDL(REG_TMP1);
  Transputer_LDIV();
  Transputer_STL(z->r2);
}

/**
 * Transputer_LLSHL - Perform left logical shift on a double-word (64-bit) number.
 *
 * @param a: Pointer to SValue struct holding the number (two 32-bit registers).
 * @param s: The number of positions to shift.
 */
void Transputer_LLSHL(SValue *a, long int s) {
  Transputer_LDL(a->r);
  Transputer_LDL(a->r2);
  Transputer_LDL(s);
  Transputer_LSHL();
  Transputer_STL(a->r);
  Transputer_STL(a->r2);
}

/**
 * Transputer_LLSHR - Perform right logical shift on a double-word (64-bit) number.
 *
 * @param a: Pointer to SValue struct holding the number (two 32-bit registers).
 * @param s: The number of positions to shift.
 */
void Transputer_LLSHR(SValue *a, long int s) {
  Transputer_LDL(a->r);
  Transputer_LDL(a->r2);
  Transputer_LDL(s);
  Transputer_LSHR();
  Transputer_STL(a->r);
  Transputer_STL(a->r2);
}

/**
 * Transputer_SLSHL - Perform left logical shift on a single-word (32-bit) number.
 *
 * @param a: Pointer to SValue struct holding the number (one 32-bit register).
 * @param s: The number of positions to shift.
 */
void Transputer_SLSHL(SValue *a, long int s) {
  Transputer_LDL(a->r);
  Transputer_XDBLE();  // Extend the single-word number to a double-word number
  Transputer_LDL(s);
  Transputer_LSHL();
  Transputer_STL(a->r);
}

/**
 * Transputer_SLSHR - Perform right logical shift on a single-word (32-bit) number.
 *
 * @param a: Pointer to SValue struct holding the number (one 32-bit register).
 * @param s: The number of positions to shift.
 */
void Transputer_SLSHR(SValue *a, long int s) {
  Transputer_LDL(a->r);
  Transputer_XDBLE();  // Extend the single-word number to a double-word number
  Transputer_LDL(s);
  Transputer_LSHR();
  Transputer_STL(a->r);
}

/**
 * Transputer_ROR - Perform right rotate on a single-word (32-bit) number.
 *
 * @param a: The register holding the number to be rotated.
 * @param s: The number of positions to rotate.
 */
void Transputer_ROR(int a, int s) {
  Transputer_LDL(a);
  Transputer_LDC(0);
  Transputer_LDL(s);
  Transputer_LSHR(); // Perform right shift
  Transputer_OR();   // Perform bitwise OR
  Transputer_STL(a); // Store the result back to the register
}

/**
 * Transputer_ROL - Perform left rotate on a single-word (32-bit) number.
 *
 * @param a: The register holding the number to be rotated.
 * @param s: The number of positions to rotate.
 */
void Transputer_ROL(int a, int s) {
  Transputer_LDL(a);
  Transputer_LDC(0);
  Transputer_LDL(s);
  Transputer_LSHL(); // Perform left shift
  Transputer_OR();   // Perform bitwise OR
  Transputer_STL(a); // Store the result back to the register
}

/*	Jump Instruction handling	*/

/* generate a jump to a label */
ST_FUNC int gjmp(int t) {
  printf("gjmp(t=%d)\n", t);
  return gjmp2(0x00, t);
}

/* generate a jump to a fixed address */
ST_FUNC void gjmp_addr(int a) {
  printf("gjmp_addr(a=%d)\n", a);

  int r;
  r = a - ind - 2; // a-L relative to org address (absolute address) - 2 bytes for instruction
  g((0x20 << 12) | (get_prefix(r) << 8) | (0x0F) << 4 | (0x0F & r));
}

// op = gt, gtu, eqc, diff
// t = absolute address
ST_FUNC int gjmp_cond(int op, int t) {
  int r;
  r = ind; // save current instruction address
  g(0x20 << 12 | 0x00 << 8 | 0x0E << 4 | 0x00); // LDL Areg, 0
  g(0xF0 | op); // CMP Areg, Breg
  r += 2; // add 2 bytes for the previous instruction
  t = gjmp2(0x00, t); // jump to target address (unconditional)
  g(0x20 << 12 | 0x00 << 8 | 0x0E << 4 | 0x00); // LDL Areg, 0 (label)
  g(0xF0 | TST); // TST Areg
  gjmp2(0x0A, r); // jump to return address if condition is false
  return t;
}


/* 	Indirect Functions 	*/

// v1 = local offset of data block from source
// v2 = same for destination
// l1 = length of v1 block in bytes
// l2 = same for v2 block
void Transputer_MOVE(int v1, int v2, int l1, int l2) {
  Transputer_LDLP(v1);
  Transputer_LDLP(v2);
  Transputer_LDC(l1);
  gen_le16(0x24FA);
  // out_op(TP_OP_MOVE);
}

/*void Transputer_GCALL()
{
	gen_le8(0xF6);
}*/

void Transputer_GAJW() {
  gen_le16(0x23FC);
}

void Transputer_AJW(int v) {
  g(0x20 | get_prefix(v));
  g(0xB | (0x0F & v));
}

void Transputer() {
  ge_l16(0x23F2);
}

void Transputer_UMINUS() {
  Transputer_NOT();
  Transputer_ADC(1);
}

void Transputer_NOP() {
  gen_le16(0x63F0);
}

void Transputer_REV() {
  g(0xF0);
}

void Transputer_LB() {
  g(0xF1);
  // out_op(TP_OP_LB);
}

void Transputer_SUM() {
  gen_le16(0x25F2);
  // out_op(TP_OP_SUM);
}

void Transputer_DIFF() {
  g(0xF4);
  // out_op(TP_OP_DIFF);
}

void Transputer_PROD() {
  g(0xF8);
  // out_op(TP_OP_PROD);
}

void Transputer_OR() {
  gen_le16(0x24FB);
  // out_op(TP_OP_OR);
}

void Transputer_DIV() {
  gen_le16(0x22FC);
  // out_op(TP_OP_DIV);
}

void Transputer_AND() {
  gen_le16(0x24F6);
  // out_op(TP_OP_AND);
}

void Transputer_LDPI() {
  gen_le16(0x21FB);
  // out_op(TP_OP_LDPI);
}

void Transputer_SHR() {
  gen_le16(0x24F0);
  // out_op(TP_OP_SHR);
}

void Transputer_LADD() {
  gen_le16(0x21F6);
  // out_op(TP_OP_LADD);
}

void Transputer_LSUB() {
  gen_le16(0x23F8);
  // out_op(TP_OP_LSUB);
}

void Transputer_LMUL() {
  gen_le16(0x23F1);
  // out_op(TP_OP_LMUL);
}

#if TARGET_CPU_DEFAULT == 9000
void Transputer_XBWORD() {
  gen_le16(0x2BF8);
}
#endif

void Transputer_XWORD() {
  gen_le16(0x23FA);
  // out_op(TP_OP_XWORD);
}

void Transputer_LDIV() {
  gen_le16(0x21FA);
  // out_op(TP_OP_LDIV);
}

void Transputer_SHL() {
  gen_le16(0x24F1);
  // out_op(TP_OP_SHL);
}

void Transputer_FPLDNLSN() {
  gen_le16(0x28FE);
  // out_op(TP_OP_FPLDNLSN);
}

void Transputer_FPSTNLSN() {
  gen_le16(0x28F8);
}

void Transputer_FPSTNLDB() {
  gen_le16(0x28F4);
}

void Transputer_FPLDNLDBI() {
  gen_le16(0x28F2);
}

void Transputer_FPLDNLSNI() {
  gen_le16(0x28F6);
}

void Transputer_FPLDNLDB() {
  gen_le16(0x28FA);
}

/*	Direct Functions 	*/

// v = offset from Wpreg
void Transputer_STL(int v) {
  g(0x20 | get_prefix(v));
  g(0xD0 | 0x0F & v);
  // out_op(TP_OP_STL);
}

void Transputer_J(int v) {
  gen_le16((0x02 << 12) | \
    (get_prefix(v) << 8) | \
    (0x0F & v));
}

void Transputer_DUP() {
  gen_le16(0x25FA);
  // out_op(TP_OP_DUP);
}

void Transputer_STNL(int v) {
  g(0x20 | get_prefix(v));
  g(0xE0 | 0x0F & v);
  // out_op(TP_OP_STNL);
}

// v = Constant 
void Transputer_LDC(int v) {
  g(0x20 | get_prefix(v));
  g(0x40 | 0x0F & v);
  // out_op(TP_OP_LDC);
}

// Load non-local address relative from Areg 
void Transputer_LDNL(int v) {
  g(0x20 | get_prefix(v));
  g(0x30 | v & 0x0F);
  // out_op(TP_OP_LDNL);
}

// v = offset from wpreg as address 
void Transputer_LDL(int v) {
  g(0x20 | get_prefix(v));
  g(0x70 | 0x0F & v);
  // out_op(TP_OP_LDL);
}

// v = offset from Areg relative address
void Transputer_LDLP(int v) {
  g(0x20 | get_prefix(v));
  g(0x10 | (0x0F & v));
  //out_op(TP_OP_LDLP);
}

void Transputer_EQC(int v) {
  g(0x20 | get_prefix(v));
  g(0xC0 | (0x0F & v));
}

void Transputer_LDNLP(int v) {
  g(0x20 | get_prefix(v));
  g(0x50 | (0x0F & v));
}

void Transputer_ADC(int v) {
  g(0x20 | get_prefix(v));
  g(0x80 | 0x0F & v);
  //out_op(TP_OP_ADC);
}

void Transputer_ADD() {
  g(0xF5);
  //out_op(TP_OP_ADD);
}

void Transputer_SUB() {
  g(0xFc);
  //out_op(TP_OP_SUB);
}

void Transputer_FPADD() {
  gen_le16(0x28F7);
}

void Transputer_FPLDNLADDSN() {
  #if TARGET_CPU_DEFAULT == 9000 || TARGET_CPU_DEFAULT == 801
  gen_le16(0x2AFA);
  #else
  Transputer_FPLDNLSN();
  //Transputer_FPRX();
  Transputer_FPADD();
  #endif
}

void Transputer_FPLDNLADDDB() {
  #if TARGET_CPU_DEFAULT == 9000 || TARGET_CPU_DEFAULT == 801
  gen_le16(0x2AF6);
  #else
  Transputer_FPLDNLDB();
  //Transputer_FPRX();
  Transputer_FPADD();
  #endif
}

void Transputer_FPLDNLMULSN() {
  #if TARGET_CPU_DEFAULT == 9000 || TARGET_CPU_DEFAULT == 801
  gen_le16(0x2AFC);
  #else
  Transputer_FPLDNLSN();
  //Transputer_FPRX();
  Transputer_FPMUL();
  #endif
}

void Transputer_FPLDNLMULDB() {
  #if TARGET_CPU_DEFAULT == 9000 || TARGET_CPU_DEFAULT == 801
  gen_le16(0x2AF8);
  #else
  Transputer_FPLDNLDB();
  //Transputer_FPRX();
  Transputer_FPMUL();
  #endif
}

void Transputer_FPGT() {
  gen_le16(0x29F4);
}

void Transputer_FPEQ() {
  gen_le16(0x29F5);
}

void Transputer_FPGE() {
  gen_le16(0x29F7);
}

void Transputer_FPREV() {
  gen_le16(0x2AF4);
}

void Transputer_FPSUB() {
  gen_le16(0x28F9);
}

void Transputer_FPMUL() {
  gen_le16(0x28FB);
}

void Transputer_FPDIV() {
  gen_le16(0x28FC);
}

void Transputer_FPEXPDEC32() {
  gen_le16(0x2DF9);
}

void Transputer_FPEXPINC32() {
  gen_le16(0x2DFA);
}

void Transputer_FPRZ() {
  gen_le16(0x2DF6);
}

void Transputer_FPRN() {
  gen_le16(0x2DF0);
}

void Transputer_FPI32TOR64() {
  gen_le16(0x29F8);
  // out_op(TP_OP_FPI32TOR64);
}

void Transputer_FPB32TOR64() {
  gen_le16(0x29FA);
  // out_op(TP_OP_FPB32TOR64);
}

void Transputer_FPI32TOR32() {
  gen_le16(0x29F6);
  // out_op(TP_OP_FPI32TOR32);
}

void Transputer_FPUEXPDEC32() {
  ge_le16(0x2DF9);
}

void Transputer_FPUCHKI32() {
  ge_le16(0x2DFE);
}

void Transputer_FPUCLRERR() {
  ge_le16(0x29FC);
}

void Transputer_FPSTNLI32() {
  ge_le16(0x29FE);
}

void Transputer_FPINT() {
  ge_le16(0x2AF1);
}

void Transputer_FPDUP() {
  ge_le16(0x2AF3);
}

/* generate an integer binary operation */
void gen_opi(int op) {
  int r, fr, ft, rt, opc;

  gv(REG_IST0);

  r = vtop -> r;
  rt = vtop -> type.t;

  switch (op) {
  case '+':
    opc = TP_OP_ADD;
    goto std_op;
  case '-':
    opc = TP_OP_SUB;
    goto std_op;
  case '&':
    opc = TP_OP_AND;
    goto std_op;
  case '^':
    opc = TP_OP_XOR;
    goto std_op;
  case '|':
    opc = TP_OP_OR;
    goto std_op;
  case '*':
    opc = TP_OP_MUL;
    goto std_op;
  case TOK_SHL:
    opc = TP_OP_SHL;
    goto std_shift;
  case TOK_SHR:
    opc = TP_OP_SHR;
    goto std_shift;
  case TOK_SAR:
    out_op(TP_OP_SHR);
    goto std_shift;
  case '/':
    opc = TP_OP_DIV;
    goto std_op;
  case '%':
    opc = TP_OP_REM;
    goto std_op;
  case TOK_UMOD:
    out_op(TP_OP_REM);
    goto std_op;
  case TOK_GT:
    opc = TP_OP_GT;
    goto std_op;
  case TOK_LT:
    opc = TP_OP_GT;
    vswap();
    goto std_op;
  case TOK_EQ:
    gv(RC_INT);
    Transputer_DIFF();
    Transputer_EQC(0);
    break;
  case TOK_NE:
    gv(RC_INT);
    Transputer_DIFF();
    break;
  case TOK_LE:
    gv(RC_INT);
    vswap();
    Transputer_GT();
    Transputer_EQC(0);
    break;
  case TOK_GE:
    gv(RC_INT);
    Transputer_LT();
    Transputer_EQC(0);
    break;
  default:
    tcc_error("unsupported binary operator");
    break;
  }

  vtop--;

  if (op >= TOK_ULT && op <= TOK_GT) {
    vset_VT_CMP(op);
  }

  return;

  std_shift:
    gv(REG_IST1);

  fr = vtop[-1].r;
  ft = vtop[-1].type.t;

  if ((rt & VT_BTYPE) == VT_PTR) {
    Transputer_LDLP(r);
  } else if ((r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
    Transputer_LDC(vtop[-1].c.i);
  } else {
    Transputer_LDL(r);
  }

  Transputer_REV();

  if ((ft & VT_BTYPE) == VT_PTR) {
    Transputer_LDLP(fr);
  } else if ((fr & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
    Transputer_LDC(vtop -> c.i);
  } else {
    Transputer_LDL(fr);
  }

  gen_le16((0x02 << 12) | (get_prefix(opc) << 8) | (0xF0 | (0x0F & opc)));

  vtop--;

  return;

  std_op:
    gv(REG_IST1);

  if ((vtop -> type.t & VT_BTYPE) == VT_LONG || (rt & VT_BTYPE) == VT_LONG) {

    fr = vtop -> r;
    ft = vtop -> type.t;

    if (op == '+') {
      Transputer_LADD_OP( & vtop[-1], & vtop[0], & vtop[-1]);
    } else if (op == '-') {
      Transputer_LSUB_OP( & vtop[-1], & vtop[0], & vtop[-1]);
    } else if (op == '/') {
      Transputer_LDIV_OP( & vtop[-1], & vtop[0], & vtop[-1]);
    } else if (op == '*') {
      Transputer_LMUL_OP( & vtop[-1], & vtop[0], & vtop[-1]);
    } else {
      // unsupported operator
      tcc_error("unsupported binary operator");
    }

  } else {
    if ((op == '+' || op == '-') && ((vtop[-1].r & VT_BTYPE) == VT_CONST)) {
      Transputer_ADC(vtop -> c.i);
    } else {
      if ((rt & VT_BTYPE) == VT_PTR) {
        Transputer_LDLP(r);
      } else if ((rt & VT_BTYPE) == VT_CONST) {
        Transputer_LDC(vtop[-1].c.i);
      } else {
        Transputer_LDL(r);
      }

      Transputer_REV();

      if ((ft & VT_BTYPE) == VT_PTR) {
        Transputer_LDLP(fr);
      } else if ((ft & VT_BTYPE) == VT_CONST) {
        Transputer_LDC(vtop -> c.i);
      } else {
        Transputer_LDL(fr);
      }

      if (op == '+' || op == '-' || op == TOK_GT) {
        Transputer_OP(get_binary_op(op));
      } else {
        // unsupported operator
        tcc_error("unsupported binary operator");
      }
    }
  }

  vtop--;

  if (op >= TOK_ULT && op <= TOK_GT) {
    if (op == TOK_ULT || op == TOK_ULE) {
      Transputer_EQC(0);
    } else {
      Transputer_EQC(-1);
    }
    vswap();
    vset_VT_CMP(op);
  }
}

static int is_zero(int i) {
  if ((vtop[i].r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
    return 0;
  if (vtop[i].type.t == VT_FLOAT)
    return (vtop[i].c.f == 0.f);
  else if (vtop[i].type.t == VT_DOUBLE)
    return (vtop[i].c.d == 0.0);

  return (vtop[i].c.ld == 0.l);
}

/* generate a floating point operation 'v = t1 op t2' instruction. The
   two operands are guaranted to have the same floating point type */
ST_FUNC void gen_opf(int op) {

  int ft, fr, r, rt;

  gv2(RC_FST0, RC_FST1);
  gv2(RC_IST0, RC_IST1);

  ft = vtop[-1].type.t;
  rt = vtop -> type.t;
  r = vtop -> r;
  fr = vtop[-1].r;

  if ((fr & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
    Transputer_LDC(ft == VT_DOUBLE ? vtop -> c.d : vtop -> c.f);
    if (ft == VT_DOUBLE)
      Transputer_FPLDNLDB();
    else
      Transputer_FPLDNLSN();
  } else {
    Transputer_LDLP(fr);
    if (ft == VT_DOUBLE)
      Transputer_FPLDNLDB();
    else
      Transputer_FPLDNLSN();
  }

  if ((rt & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST)
    Transputer_LDC(rt == VT_DOUBLE ? vtop -> c.d : vtop -> c.f);
  else
    Transputer_LDLP(r);

  switch (op) {
  case '+':
    if (rt == VT_DOUBLE)
      Transputer_FPLDNLADDDB();
    else
      Transputer_FPLDNLADDSN();
    break;
  case '-':
    Transputer_FPSUB();
    break;
  case '*':
    if (rt == VT_DOUBLE)
      Transputer_FPLDNLMULDB();
    else
      Transputer_FPLDNLMULSN();
    break;
  case '/':
    Transputer_FPREV();
    Transputer_FPDIV();
    break;
  case '%':
    Transputer_FPREM();
    break;
  case TOK_GT:
    Transputer_FPGT();
    break;
  case TOK_LE:
    Transputer_FPGT();
    Transputer_EQC(0);
    break;
  case TOK_LT:
    Transputer_FPREV();
    Transputer_FPGT();
    vswap();
    break;
  case TOK_GE:
    Transputer_FPGE();
    break;
  case TOK_EQ:
    Transputer_FPEQ();
    break;
  case TOK_NE:
    Transputer_FPGT();
    Transputer_EQC(0);
    break;
  default:
    tcc_error("unsupported binary operator");
    break;
  }

  vtop--;
}

/* convert fp to int 't' type */
/* XXX: handle long long case */
ST_FUNC void gen_cvt_ftoi(int t) {
  int r, r2;

  r = fpr(gv(RC_INT));
  r2 = intr(vtop -> r = get_reg(RC_INT));

  if ((vtop -> type.t & VT_BTYPE) == VT_INT) {
    Transputer_FPINT();
    Transputer_FPUCLRERR();
    Transputer_FPUCHKI32();
    Transputer_FPCHKERR();
    Transputer_LDLP(vtop -> r);
    Transputer_FPSTNLI32();
  } else if ((vtop -> type.t & VT_BTYPE) == VT_LONG) {
    Transputer_FPRZ();
    Transputer_FPINT();
    Transputer_LDLP(vtop -> r);
    Transputer_FPDUP();
    Transputer_DUP();
    Transputer_FPSTNLI32();
    Transputer_LDNLP(1);
    Transputer_FPUEXPDEC32();
    Transputer_FPSTNLI32();
  }

  vtop -> r = REG_IRET;
}

/* convert integers to fp 't' type. Must handle 'int', 'unsigned int'
   and 'long long' cases. */
ST_FUNC void gen_cvt_itof(int t) {
  fpu_push();
  gv(RC_INT);

  if (vtop -> type.t == (VT_INT | VT_FLOAT)) {
    Transputer_LDLP(vtop -> r);
    Transputer_FPI32TOR32();
    vtop -> r2 = VT_FLOAT;
  } else if (vtop -> type.t == (VT_INT | VT_DOUBLE)) {
    Transputer_LDLP(vtop -> r);
    Transputer_FPI32TOR64();
    vtop -> r2 = VT_DOUBLE;
  } else if (vtop -> type.t == (VT_LLONG | VT_DOUBLE)) {
    Transputer_LDLP(vtop -> r);
    Transputer_DUP();
    Transputer_FPB32TOR64();
    Transputer_LDNLP(1);
    Transputer_FPI32TOR64();
    Transputer_FPEXPINC32();
    Transputer_FPADD();
    vtop -> r2 = VT_DOUBLE;
  }
}

ST_FUNC void gen_cvt_csti(int t) {

  printf("gen_cvt_csti(t=%d)\n", t);

  gv(RC_INT);

  if ((t & VT_BTYPE) == VT_SHORT) {
    #if TARGET_CPU_DEFAULT == 9000
    Transputer_XSWORD();
    #else
    Transputer_REV();
    Transputer_LDC(4);
    Transputer_XWORD();
    #endif
  } else if ((t & VT_BTYPE) == VT_BYTE) {
    #if TARGET_CPU_DEFAULT == 9000
    Transputer_XBWORD();
    #else
    Transputer_REV();
    Transputer_LDC(4);
    Transputer_XWORD();
    #endif
  } else if ((t & VT_BTYPE) == VT_INT) {
    Transputer_REV();
    Transputer_LDC(4);
    Transputer_XWORD();
    vtop -> r2 = VT_INT;
  }

  vtop -> type.t = VT_INT;
  vtop -> r2 = VT_CONST;

}

/* convert from one floating point type to another */
ST_FUNC void gen_cvt_ftof(int t) {

  printf("gen_cvt_ftof(t=%d)\n", t);

  if ((vtop -> type.t & VT_BTYPE) == VT_DOUBLE &&
    (t & VT_BTYPE) == VT_FLOAT) {
    // convert double to float
    vtop -> type.t = VT_FLOAT;
    vtop -> r2 = VT_CONST;
    gv(RC_FLOAT);
    Transputer_FPR64TOR32();

  } else if ((vtop -> type.t & VT_BTYPE) == VT_FLOAT &&
    (t & VT_BTYPE) == VT_DOUBLE) {
    // convert float to double
    vtop -> type.t = VT_DOUBLE;
    vtop -> r2 = 0;
    gv(RC_FLOAT);
    Transputer_FPR32TOR64();
  } else {
  }

}

/* computed goto support */
ST_FUNC void ggoto(void) {
  printf("ggoto()\n");

  gcall_or_jmp(1);
  vtop--;
}

ST_FUNC void g(int c) {
  int ind1;
  ind1 = ind + 1;
  if (ind1 > cur_text_section -> data_allocated)
    section_realloc(cur_text_section, ind1);
  cur_text_section -> data[ind] = c;
  ind = ind1;
}

ST_FUNC void gen_le16(int v) {
  g(v);
  g(v >> 8);
}

ST_FUNC void gen_le32(int c) {
  g(c);
  g(c >> 8);
  g(c >> 16);
  g(c >> 24);
}

ST_FUNC void o(unsigned int c) {
  while (c) {
    g(c);
    c = c >> 8;
  }
}

/* VLA support functions */

/* Save the workspace pointer onto the stack and return the location of its address */
ST_FUNC void gen_vla_sp_save(int addr) {
    Transputer_LDLP(REG_WSP);
    Transputer_STLP(addr);
}

/* Restore the workspace pointer from a location on the stack */
ST_FUNC void gen_vla_sp_restore(int addr) {
    Transputer_LDLP(addr);
    Transputer_STLP(REG_WSP);
}

/* Subtract from the workspace pointer, and push the resulting value onto the stack */
ST_FUNC void gen_vla_alloc(CType * type, int align) {
    int size;

    /* Calculate size of the array */
    if (type->t & VT_VLA) {
        /* The size is in bytes, so we need to divide by the size of each element */
        gv(RC_INT);
        Transputer_LDL(REG_SP);
        Transputer_SUB();
        Transputer_STL(REG_SP);
        size = type->vla_size / type_size(type->next);
    } else {
        size = type->array_size;
    }

    /* Subtract from the workspace pointer */
    Transputer_LDL(REG_WSP);
    Transputer_LDLP(REG_WSP);
    Transputer_SUB();
    Transputer_STLP(REG_WSP);

    /* Push the resulting value onto the stack */
    Transputer_LDC(size);
    Transputer_PUSH();
}

#endif
/* end of transputer code generator */
