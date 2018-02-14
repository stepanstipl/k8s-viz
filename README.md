# k8s-viz

*Visualize what's happening in you Kubernetes cluster.*

K8s-viz is Elm webapp to visualise what's going on in your cluster. I've found
it rather difficult sometimes to properly explain relationships between
various resources in Kubernetes world (like Deployments, ReplicaSets,
Services...) so hopefully this tool will help.

![Screencast](docs/screencast.gif)

## Usage

- Setup K8s and make sure kubectl works as expected
- Clone this repo
  ```
  git clone https://github.com/stepanstipl/k8s-viz
  ```
- Run kubectl proxy from the project directory
  ```
  cd k8s-viz
  kubectl proxy --www=./ --www-prefix=/ --api-prefix=/k8s-api/
  ```
- Open k8s-viz dashboard on `localhost:8001`

## Build

- Build Elm webapp
  ```
  elm-make Main.elm --output main.js
  ```

- Build css
  ```
  sass --update sass:css
  ```
## Credit

This tools is inspired by Brendan Burns' visualiser -
[brendandburns/gcp-live-k8s-visualizer](https://github.com/brendandburns/gcp-live-k8s-visualizer) .
