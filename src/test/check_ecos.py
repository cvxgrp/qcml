import os
import shutil
import subprocess
import platform
import re

FILE_PATH = os.path.dirname(__file__)
DEFAULT = os.path.join(FILE_PATH, "../../ecos")
ECOS_PATH = os.environ.get("ECOS_PATH")
ECOS_PATH = ECOS_PATH if ECOS_PATH else DEFAULT


def make_and_execute_ecos_solve(problem_name, c_code):
    with open("%s/main.c" % problem_name, "w") as f:
        f.write(c_code)

    os.chdir(problem_name)
    print "Running make...."
    subprocess.check_output(["make"])
    if platform.system() == 'Linux':
        cmd = ["cc", "-O3", "main.c",
                "-L%s" % ECOS_PATH,
                "-I%s/include" % ECOS_PATH, "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                "-lecos", "-lm", "-lrt", "test_problem.o", "qcml_utils.o", "-o","main"]
    else:
        cmd = ["cc", "-O3", "main.c",
                "-L%s" % ECOS_PATH,
                "-I%s/include" % ECOS_PATH, "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                "-lecos", "-lm", "test_problem.o", "qcml_utils.o", "-o","main"]
    print ' '.join(cmd)
    try:
        subprocess.check_output(cmd)
    except subprocess.CalledProcessError:
        print ""
        print "This test fails because it cannot find ECOS,"
        print "please set your ECOS_PATH environment variable."
        print ""
        print "    export ECOS_PATH=/PATH/TO/ECOS"
        print ""
        raise

    try:
        output = subprocess.check_output(["./main"])
    except:
        print ""
        print "There is a bug in the compiled C code."
        print ""
        raise
    objval = re.findall(r'Objective value at termination of C program is [-]?(\d+\.\d*)', output)
    os.chdir("..")
    shutil.rmtree("%s/test_problem" % os.getcwd())

    assert len(objval) == 1
    return float(objval[0])
