import sijuda

while True:
    input_text = input('SIJUDA > ')
    result, error = sijuda.run(input_text)
    if error:
        print(error.to_string())
    elif result:
        print(result)