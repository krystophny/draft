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
    ]
    for field in fields:
        config[field] = next(lines).strip().split()[0]
    return config


def remove_unused_fields(config: dict) -> dict:
    unused_fields = ["n0", "mthnum", "Mtmin", "Mtmax", "Mtnum", "calcflux"]
    for field in unused_fields:
        del config[field]
    return config


def add_missing_fields(config: dict) -> dict:
    config["runmode"] = "\"transport\""
    config["nonlin"] = "F"
    config["efac"] = "1.0d0"
    config["bfac"] = "1.0d0"
    config["inp_swi"] = 9
    config["orbit_mode_avg"] = 0
    config["orbit_mode_transp"] = 0
    config["vsteps"] = 256
    config["comptorque"] = "F"
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
