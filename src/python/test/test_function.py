from qcml.codegens.function import PythonFunction, MatlabFunction, CFunction

py_helloWorld = PythonFunction("hello")
py_hello = """def hello(x, y, z, a):
    # documenting the function
    # this function just tests
    # that hello world
    # really works as expected

    # hello world
    # this function just tests
    # that hello world
    # really works as expected
    print 'hello world'
    c = x + y + z
    a = 1"""

c_helloWorld = CFunction("hello", ret_type="int")
c_hello = """int hello(x, y, z, a) {
    // documenting the function
    // this function just tests
    // that hello world
    // really works as expected

    // hello world
    // this function just tests
    // that hello world
    // really works as expected
    print 'hello world'
    c = x + y + z
    a = 1
}"""

matlab_helloWorld = MatlabFunction("hello", ret_args=["a", "b", "c"])
matlab_hello = """function [a, b, c] = hello(x, y, z, a)
    % documenting the function
    % this function just tests
    % that hello world
    % really works as expected

    % hello world
    % this function just tests
    % that hello world
    % really works as expected
    print 'hello world'
    c = x + y + z
    a = 1
end"""

def test_indent():
    # four space indentation
    assert(py_helloWorld.indent == '    ')
    assert(c_helloWorld.indent == '    ')


def test_comment():
    # '#' comment in pyhton
    assert(py_helloWorld.comment == '#')
    assert(c_helloWorld.comment == '//')


def string_gen():
    yield "this function just tests"
    yield "that hello world"
    yield "really works as expected"

def arg_gen():
    yield "x"
    yield "y"
    yield "z"

def code_gen():
    yield "c = x + y + z"
    yield "a = 1"


def source_match(func_obj, expected):
    func_obj.document("documenting the function")
    func_obj.document(string_gen())

    func_obj.newline()

    func_obj.add_comment("hello world")
    func_obj.add_comment(string_gen())

    func_obj.add_arguments(arg_gen())
    func_obj.add_arguments("a")

    func_obj.add_lines("print 'hello world'")
    func_obj.add_lines(code_gen())

    print func_obj.source
    print expected
    assert(func_obj.source == expected)

def test_source():
    test_cases = (
        (py_helloWorld, py_hello),
        (c_helloWorld, c_hello),
        (matlab_helloWorld, matlab_hello)
    )
    for func, expected in test_cases:
        yield source_match, func, expected

