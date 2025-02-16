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
        "orbit_mode_avg",
        "orbit_mode_transp",
        "vsteps",
        "intoutput"
    ]
    for field in fields:
        try:
            config[field] = next(lines).strip().split()[0]
        except:
            print(f"Field {field} not found in input file")
    return config


def remove_unused_fields(config: dict) -> dict:
    unused_fields = ["n0", "mthnum", "Mtmin", "Mtmax", "Mtnum", "calcflux"]
    for field in unused_fields:
        del config[field]
    return config


def add_missing_fields(config: dict) -> dict:
    config["runmode"] = "\"transport\""
    if not "nonlin" in config:
        config["nonlin"] = "F"
    if not "efac" in config:
        config["efac"] = "1.0d0"
    if not "bfac" in config:
        config["bfac"] = "1.0d0"
    if not "inp_swi" in config:
        config["inp_swi"] = 9
    if not "orbit_mode_avg" in config:
        config["orbit_mode_avg"] = 0
    if not "orbit_mode_transp" in config:
        config["orbit_mode_transp"] = 0
    if not "vsteps" in config:
        config["vsteps"] = 256
    if not "intoutput" in config:
        config["intoutput"] = "F"
    return config


def correct_changed_fields(config: dict) -> dict:
    config["epsmn"] = "1.0d0"
    return config


def print_new_input(config: dict):
    print(
        "! Input file for NEO-RT - Version 1.3\n"
        "!\n"
        "!\n"
        "&params"
    )
    for key, value in config.items():
        if value == "T": value = ".true."
        if value == "F": value = ".false."
        print(f"    {key} = {value}")
    print("/")

if __name__ == "__main__":
    main()
