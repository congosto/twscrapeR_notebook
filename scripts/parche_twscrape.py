# parche_twscrape.py
import json
import re

def script_url(k: str, v: str):
    return f"https://abs.twimg.com/responsive-web/client-web/{k}.{v}.js"

def patched_get_scripts_list(text: str):
    scripts = text.split('e=>e+"."+')[1].split('[e]+"a.js"')[0]
    try:
        for k, v in json.loads(scripts).items():
            yield script_url(k, f"{v}a")
    except json.decoder.JSONDecodeError:
        fixed_scripts = re.sub(
            r'([,\{])(\s*)([\w]+_[\w_]+)(\s*):',
            r'\1\2"\3"\4:',
            scripts
        )
        for k, v in json.loads(fixed_scripts).items():
            yield script_url(k, f"{v}a")

# 👇 aplicar el parche antes de usar twscrape
from twscrape import xclid
xclid.get_scripts_list = patched_get_scripts_list
