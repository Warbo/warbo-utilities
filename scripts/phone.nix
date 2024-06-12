{
  bash,
  iptables,
  wrap,
}:

wrap {
  name = "phone";
  paths = [
    bash
    iptables
  ];
  script = ''
    #!${bash}/bin/bash
    set -e

    [[ -n "$DEVICE" ]] || DEVICE="enp0s29f0u2"
    echo "Using DEVICE '$DEVICE'"                                    1>&2
    echo "NOTE: The OpenMoko should appear as '$DEVICE' in ifconfig" 1>&2

    [[ -n "$ADDRESS" ]] || ADDRESS="192.168.1.200"
    echo "Using ADDRESS '$ADDRESS'" 1>&2
    echo "NOTE: This is the static IP we'll set on our end" 1>&2

    # "Easy" method; unreliable
    #sudo ip address add "$ADDRESS"/24 dev "$DEVICE"
    #sudo ip link set dev "$DEVICE" up

    # "Harder" method; seems to work, also forwards the Internet connection
    # shellcheck disable=SC2001
    ZERO=$(echo "$ADDRESS" | sed -e 's/[0-9]*$/0/g')
    sudo iptables -A POSTROUTING -t nat -j MASQUERADE -s "$ZERO"/24
    sudo sysctl -w net.ipv4.ip_forward=1
    sudo ip addr add "$ADDRESS"/24 dev "$DEVICE"

    echo "Phone should now be available at 192.168.1.202"                 1>&2
    echo "If not, run 'ifconfig' on the phone to make sure that's its IP" 1>&2
  '';
}
