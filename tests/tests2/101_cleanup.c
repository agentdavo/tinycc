#include <tcclib.h>

void check2(char **hum);

void check(int *j)
{
    char * __attribute__ ((cleanup(check2))) stop_that = "wololo";
    int chk = 0;

    {
	char * __attribute__ ((cleanup(check2))) stop_that = "plop";

	{
	  non_plopage:
	    printf("---- %d\n", chk);
	}
	if (!chk) {
	    chk = 1;
	    goto non_plopage;
	}
    }

    {
	char * __attribute__ ((cleanup(check2))) stop_that = "tata !";

	goto out;
	stop_that = "titi";
    }
  again:
    chk = 2;
    {
	char * __attribute__ ((cleanup(check2))) cascade1 = "1";
	{
	    char * __attribute__ ((cleanup(check2))) cascade2 = "2";
	    {
		char * __attribute__ ((cleanup(check2))) cascade3 = "3";

		goto out;
		cascade3 = "nope";
	    }
	}
    }
  out:
    if (chk != 2)
	goto again;
    {
	{
	    char * __attribute__ ((cleanup(check2))) out = "last goto out";
	    ++chk;
	    if (chk != 3)
		goto out;
	}
    }
    return;
}

void check_oh_i(char *oh_i)
{
    printf("c: %c\n", *oh_i);
}

void goto_hell(double *f)
{
    printf("oo: %f\n", *f);
}

char *test()
{
    char *__attribute__ ((cleanup(check2))) str = "I don't think this should be print(but gcc got it wrong too)";

    return str;
}

void test_ret_subcall(char *that)
{
    printf("should be print before\n");
}

void test_ret()
{
    char *__attribute__ ((cleanup(check2))) that = "that";
    return test_ret_subcall(that);
}

void test_ret2()
{
  char *__attribute__ ((cleanup(check2))) that = "-that";
  {
    char *__attribute__ ((cleanup(check2))) that = "this should appear only once";
  }
  {
    char *__attribute__ ((cleanup(check2))) that = "-that2";
    return;
  }
}

int main()
{
    int i __attribute__ ((__cleanup__(check))) = 0, not_i;
    int chk = 0;

    {
	__attribute__ ((__cleanup__(check_oh_i))) char oh_i = 'o', o = 'a';
    }

  naaaaaaaa:
    if (!chk) {
	__attribute__ ((__cleanup__(check_oh_i))) char oh_i = 'f';
	double __attribute__ ((__cleanup__(goto_hell))) f = 2.6;

	chk = 1;
	goto naaaaaaaa;
    }
    i = 105;
    printf("because what if free was call inside cleanup function\n", test());
    test_ret();
    test_ret2();
    return i;
}

void check2(char **hum)
{
    printf("str: %s\n", *hum);
}