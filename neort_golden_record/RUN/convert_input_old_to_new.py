import sys
import f90nml


def main():
    old_input = sys.argv[1]

    with open(old_input, "r") as f:
        lines = f.readlines()
    lines = skip_comments(lines)
    config = read_old_input(lines)
    config = remove_unused_fields(config)
    config = add_missing_fields(config)
    config = correct_changed_fields(config)
    print_new_input(config)


def skip_comments(lines: list) -> list:
    comment_symbols = ["!", "#"]
    for line in lines:
        if line.strip()[0] in comment_symbols:
            continue
        else:
            yield line


def read_old_input(lines: list) -> dict:
    config = {}
    fields = [
        "s",
        "M_t",
        "qs",
        "ms",
        "n0",
        "vth",
        "epsmn",
        "m0",
        "mph",
        "mth",
        "mthnum",
        "Mtmin",
        "Mtmax",
        "Mtnum",
        "supban",
        "magdrift",
        "nopassing",
        "calcflux",
        "noshear",
        "pertfile",
        "odeint",
        "nonlin",
        "bfac",
        "efac",
        "inp_swi",
        "vsteps",
    ]
    for field in fields:
        try:
            config[field] = next(lines).strip().split()[0]
        except:
            print(f"Field {field} not found in input file")
    return config


def remove_unused_fields(config: dict) -> dict:
    unused_fields = [
        "n0",
        "mth",
        "mthnum",
        "Mtmin",
        "Mtmax",
        "Mtnum",
        "calcflux",
        "supban",
    ]
    for field in unused_fields:
        del config[field]
    return config


def add_missing_fields(config: dict) -> dict:
    config["comptorque"] = "F"
    if not "nonlin" in config:
        config["nonlin"] = "F"
    if not "efac" in config:
        config["efac"] = "1.0d0"
    if not "bfac" in config:
        config["bfac"] = "1.0d0"
    if not "inp_swi" in config:
        config["inp_swi"] = 9
    if not "vsteps" in config:
        config["vsteps"] = 256
    return config


def correct_changed_fields(config: dict) -> dict:
    config["epsmn"] = "1.0d0"
    return config


def print_new_input(config: dict):
    print("! Input file for NEO-RT - Version 1.3\n" "!\n" "!\n" "&params")
    for key, value in config.items():
        if value == "T":
            value = ".true."
        if value == "F":
            value = ".false."
        print(f"    {key} = {value}")
    print("/")


if __name__ == "__main__":
    main()
