import { useState } from "react";
import { createRoot } from "react-dom/client";
import { renderCount } from "./gen/counter";

function App() {
  const [count, setCount] = useState(0);
  return (
    <div style={{ fontFamily: "system-ui", padding: "2rem" }}>
      {renderCount(count)}
      <button type="button" onClick={() => setCount((c) => c + 1)}>
        Increment
      </button>
    </div>
  );
}

createRoot(document.getElementById("root")!).render(<App />);
