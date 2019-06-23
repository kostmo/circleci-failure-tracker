function setDefault(obj, prop, deflt) {
    return obj.hasOwnProperty(prop) ? obj[prop] : (obj[prop] = deflt);
}
