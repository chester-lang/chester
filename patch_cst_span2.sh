sed -i '' '/| Error _ span => span/c\
  | Error _ span => span\
  | _ => empty_span\
' theories/CST.v
