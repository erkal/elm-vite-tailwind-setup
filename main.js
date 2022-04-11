import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

function sendEdgeDataToElmApp() {
  app.ports.outEdgeCircleCoordinatesForSvgPositioning.send(
    Array.from(document.getElementsByClassName("out-edge-circle")).map((el) => {
      return { id: el.id, offsetTop: el.offsetTop };
    })
  );
}

sendEdgeDataToElmApp();

new MutationObserver(function () {
  console.log("callback that runs when observer is triggered");
  sendEdgeDataToElmApp();
}).observe(root, { subtree: true, childList: true });
