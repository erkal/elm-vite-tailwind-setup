import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

const outEdgesCircles = Array.from(
  document.getElementsByClassName("out-edge-circle")
);
app.ports.outEdgeCircleCoordinatesForSvgPositioning.send(
  outEdgesCircles.map((el) => {
    return {
      id: el.id,
      offsetTop: el.offsetTop,
    };
  })
);

new MutationObserver(function () {
  console.log("callback that runs when observer is triggered");
  const outEdgesCircles = Array.from(
    document.getElementsByClassName("out-edge-circle")
  );
  app.ports.outEdgeCircleCoordinatesForSvgPositioning.send(
    outEdgesCircles.map((el) => {
      return {
        id: el.id,
        offsetTop: el.offsetTop,
      };
    })
  );
}).observe(document.querySelector("#html-canvas"), {
  subtree: true,
  childList: true,
});
