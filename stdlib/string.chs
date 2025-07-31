fn memset(p *char, byte char, size int) -> *char {
    var i int = 0;
    while(i < size) {
        *(p + i) = byte;
        i = i + 1;
    }
    return p;
}

fn strlen(s *char) -> int {
    var end *char = s;
    while((*end) != 0) {
        end = end + 1;
    }
    return end - s;
}
