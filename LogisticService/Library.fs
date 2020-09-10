namespace LogisticService

exception InvalidDataException of string

module RoutePlan =

    type Goods =
        | A
        | B

    type Destination =
        | DropOffA
        | DropOffB
        | Port
        | Factory

    type Transporter =
        { DropOff: int
          Back: int
          Destination: Destination }

    type GoodsLocator =
        { Factory: List<Goods>
          A: int
          B: int
          Port: int }

    let setTruckStatus goods currentHour =
        match goods with
        | A ->
            { DropOff = currentHour + 1
              Back = currentHour + 2
              Destination = Port }
        | B ->
            { DropOff = currentHour + 5
              Back = currentHour + 10
              Destination = DropOffB }

    let dispatchTrucksFromFactory trucksAtFactory (goodsAtFactory: List<Goods>) currentHour =
        let truckCount = (List.length trucksAtFactory)
        (List.map (fun g -> setTruckStatus g currentHour) goodsAtFactory.[..(truckCount - 1)],
         goodsAtFactory.[truckCount..])

    let unloadTrucks trucks (goodsLocator: GoodsLocator) =
        if List.isEmpty trucks then
            (trucks, goodsLocator)
        else
            let unload (trucks: List<Transporter>): GoodsLocator =
                let count =
                    List.countBy (fun t -> t.Destination) trucks

                let newStock dest =
                    if List.exists (fun g -> fst g = dest) count
                    then snd (List.find (fun g -> fst g = dest) count)
                    else 0

                { goodsLocator with
                      B = newStock DropOffB + goodsLocator.B
                      Port = newStock Port + goodsLocator.Port }

            let setNewDestination truck =
                match truck.Destination with
                | Port -> { truck with Destination = Factory }
                | DropOffB -> { truck with Destination = Factory }
                | _ -> truck

            let goods = trucks |> unload
            (List.map setNewDestination trucks, goods)

    let dispatchShipFromPort ship goodsAtPort hour =
        if ship.Back <= hour then
            if goodsAtPort > 0 then
                ({ ship with
                       Destination = DropOffA
                       DropOff = hour + 4
                       Back = hour + 8 },
                 goodsAtPort - 1)
            else
                (ship, 0)
        else
            (ship, goodsAtPort)

    let unloadShip ship goodsAtDestination hour =
        if ship.DropOff = hour
        then ({ ship with Destination = Port }, goodsAtDestination + 1)
        else (ship, goodsAtDestination)

    let rec simulate numberOfGoodsToDeliver (goodsLocator: GoodsLocator) trucks ship hour =
        
        let trucksEnroute =
            List.filter (fun t -> t.Back <> hour && t.DropOff <> hour) trucks

        let (trucksHeadingToDestination, goodsAtFactory) =
            dispatchTrucksFromFactory (List.filter (fun t -> t.Back = hour) trucks) goodsLocator.Factory hour

        let (trucksHeadingBack, goodsAfterUnloadingTrucks) =
            unloadTrucks (List.filter (fun t -> t.DropOff = hour) trucks) goodsLocator

        let (dispatchedShip, goodsAtPort) =
            dispatchShipFromPort ship goodsAfterUnloadingTrucks.Port hour

        let (unloadedShip, goodsAtA) =
            unloadShip dispatchedShip goodsLocator.A hour

        let updatedGoodsLocator =
            { Factory = goodsAtFactory
              A = goodsAtA
              B = goodsAfterUnloadingTrucks.B
              Port = goodsAtPort }

        match updatedGoodsLocator with
        | l when l.A + l.B = numberOfGoodsToDeliver -> hour
        | _ ->
            simulate
                numberOfGoodsToDeliver
                updatedGoodsLocator
                (trucksHeadingToDestination
                 @ trucksHeadingBack
                 @ trucksEnroute)
                unloadedShip
                (hour + 1)

    let public plan (goods: string []) =

        let trucks =
            [ { DropOff = -1
                Back = 0
                Destination = Factory }
              { DropOff = -1
                Back = 0
                Destination = Factory } ]

        let ship =
            { DropOff = -1
              Back = 0
              Destination = Port }

        let goodsLocator =
            { GoodsLocator.Factory =
                  [ for i in goods ->
                      match i with
                      | "A" -> A
                      | "B" -> B
                      | _ -> raise (InvalidDataException("No such goods")) ]
              GoodsLocator.B = 0
              GoodsLocator.A = 0
              GoodsLocator.Port = 0 }

        simulate goods.Length goodsLocator trucks ship 0
